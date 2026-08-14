# ============================================================================
# 00_functions_provisioning.R
# ----------------------------------------------------------------------------
# Utility functions for raw data provisioning: download, integrity
# verification, format conversion, archival packaging, and reference
# data unification. Used by:
#   - 00_01_download_raw_sources.qmd  (download, hash, extract, cleanup)
#   - 00_02_convert_to_parquet.qmd    (convert, verify, unify, package)
#
# All functions use explicit `package::function` notation to remain
# self-contained and executable in any environment.
#
# Required packages (must be installed before sourcing):
#   curl, digest, dplyr, fs, purrr, readxl, haven, arrow, stringr
#
# Progress is tracked by the calling scripts via gt tables; functions
# are silent during normal operation to avoid noise in Quarto renders.
# ============================================================================

# Verify critical dependencies are installed before proceeding
.required_pkgs <- c("curl", "digest", "dplyr", "fs", "purrr", "stringr")
.missing_pkgs <- .required_pkgs[!vapply(.required_pkgs, requireNamespace,
                                         logical(1), quietly = TRUE)]
if (length(.missing_pkgs) > 0) {
  stop(
    "00_functions_provisioning.R requires the following packages: ",
    paste(.missing_pkgs, collapse = ", "),
    ". Install them via install.packages() or add them to p_load() in ",
    "the calling script.",
    call. = FALSE
  )
}
rm(.required_pkgs, .missing_pkgs)


# ============================================================================
# S1  Download & Network
# ============================================================================


# ----------------------------------------------------------------------------
# download_with_retry
# ----------------------------------------------------------------------------
# Purpose:
#   Robust file download with automatic retry on transient network errors.
#   Wraps curl::curl_download with a configurable retry strategy.
#
# Arguments:
#   url           Character. Direct URL to the file to download.
#   dest_path     Character. Local destination path (will be overwritten).
#   max_attempts  Integer. Maximum number of download attempts before
#                 returning failure. Default 3.
#   timeout_sec   Integer. Per-attempt timeout in seconds. Default 120.
#
# Returns:
#   A 1-row tibble with columns:
#     url, dest_path, success (lgl), attempts (int), error_message (chr)
# ----------------------------------------------------------------------------
download_with_retry <- function(url,
                                dest_path,
                                max_attempts = 3,
                                timeout_sec = 120) {

  result <- dplyr::tibble(
    url = url,
    dest_path = dest_path,
    success = FALSE,
    attempts = 0L,
    error_message = NA_character_
  )

  for (attempt in seq_len(max_attempts)) {
    result$attempts <- attempt

    download_ok <- tryCatch(
      {
        h <- curl::new_handle()
        curl::handle_setopt(h, timeout = timeout_sec, followlocation = TRUE)
        curl::curl_download(url = url, destfile = dest_path, handle = h,
                            quiet = TRUE)
        TRUE
      },
      error = function(e) {
        result$error_message <<- conditionMessage(e)
        FALSE
      }
    )

    if (isTRUE(download_ok) && file.exists(dest_path) &&
        file.info(dest_path)$size > 0) {
      result$success <- TRUE
      result$error_message <- NA_character_
      return(result)
    }

    Sys.sleep(2 ^ attempt)
  }

  result
}


# ----------------------------------------------------------------------------
# download_batch
# ----------------------------------------------------------------------------
# Purpose:
#   Download multiple files with progress messages. Wraps
#   download_with_retry() for each file in the manifest.
#
# Arguments:
#   manifest_df   Tibble. Must contain columns: filename, url, staged_path,
#                 needs_download.
#   max_attempts  Integer. Passed to download_with_retry(). Default 3.
#   timeout_sec   Integer. Passed to download_with_retry(). Default 120.
#
# Returns:
#   The input tibble with additional columns: success, attempts,
#   error_message.
# ----------------------------------------------------------------------------
download_batch <- function(manifest_df, max_attempts = 3, timeout_sec = 120) {

  to_download <- manifest_df |> dplyr::filter(needs_download)

  if (nrow(to_download) == 0) {
    return(manifest_df |>
             dplyr::mutate(success = TRUE, attempts = 0L,
                           error_message = NA_character_))
  }

  results <- purrr::map_dfr(seq_len(nrow(to_download)), function(i) {
    row <- to_download[i, ]
    download_with_retry(
      url = row$url,
      dest_path = row$staged_path,
      max_attempts = max_attempts,
      timeout_sec = timeout_sec
    )
  })

  manifest_df |>
    dplyr::left_join(
      results |> dplyr::select(dest_path, success, attempts, error_message),
      by = c("staged_path" = "dest_path")
    ) |>
    dplyr::mutate(
      success = dplyr::if_else(is.na(success) & !needs_download, TRUE, success),
      attempts = dplyr::if_else(is.na(attempts), 0L, attempts)
    )
}


# ============================================================================
# S2  Hashing & Verification
# ============================================================================


# ----------------------------------------------------------------------------
# compute_file_hash
# ----------------------------------------------------------------------------
compute_file_hash <- function(path) {

  if (!file.exists(path)) {
    return(NA_character_)
  }

  digest::digest(object = path, algo = "sha256", file = TRUE,
                 serialize = FALSE)
}


# ----------------------------------------------------------------------------
# verify_hash_sha256
# ----------------------------------------------------------------------------
verify_hash_sha256 <- function(path, expected_hash) {

  observed <- compute_file_hash(path)

  status <- dplyr::case_when(
    is.na(observed)                                ~ "missing",
    is.na(expected_hash)                           ~ "no_expect",
    tolower(observed) == tolower(expected_hash)    ~ "match",
    TRUE                                           ~ "mismatch"
  )

  dplyr::tibble(
    path = path,
    expected_hash = expected_hash,
    observed_hash = observed,
    status = status
  )
}


# ----------------------------------------------------------------------------
# verify_hashes_batch
# ----------------------------------------------------------------------------
verify_hashes_batch <- function(files_df) {

  results <- purrr::map2_dfr(
    files_df$target_path,
    files_df$sha256,
    function(p, h) {
      verify_hash_sha256(p, h)
    }
  )

  results
}


# ============================================================================
# S3  Archive Extraction & Packaging
# ============================================================================


# ----------------------------------------------------------------------------
# extract_zip_selective
# ----------------------------------------------------------------------------
extract_zip_selective <- function(zip_path, internal_filter, dest_dir) {

  fs::dir_create(dest_dir)

  zip_contents <- utils::unzip(zip_path, list = TRUE)

  matching <- zip_contents |>
    dplyr::filter(stringr::str_detect(Name, internal_filter)) |>
    dplyr::pull(Name)

  if (length(matching) == 0) {
    return(dplyr::tibble(internal_path = character(),
                         dest_path = character(),
                         extracted = logical()))
  }

  results <- purrr::map_dfr(matching, function(internal_path) {

    flat_name <- basename(internal_path)
    dest_path <- file.path(dest_dir, flat_name)

    extracted_ok <- tryCatch(
      {
        utils::unzip(zip_path,
                     files = internal_path,
                     exdir = tempdir(),
                     overwrite = TRUE,
                     junkpaths = FALSE)
        src <- file.path(tempdir(), internal_path)
        if (file.exists(src)) {
          fs::file_copy(src, dest_path, overwrite = TRUE)
          fs::file_delete(src)
          TRUE
        } else {
          FALSE
        }
      },
      error = function(e) FALSE
    )

    dplyr::tibble(
      internal_path = internal_path,
      dest_path = dest_path,
      extracted = extracted_ok
    )
  })

  results
}


# ----------------------------------------------------------------------------
# package_to_zip
# ----------------------------------------------------------------------------
package_to_zip <- function(files, archive_path, working_dir) {

  fs::dir_create(dirname(archive_path))

  if (file.exists(archive_path)) {
    fs::file_delete(archive_path)
  }

  rel_files <- purrr::map_chr(files, function(f) {
    if (fs::path_has_parent(f, working_dir)) {
      as.character(fs::path_rel(f, start = working_dir))
    } else {
      basename(f)
    }
  })

  old_wd <- setwd(working_dir)
  on.exit(setwd(old_wd), add = TRUE)

  archive_abs <- normalizePath(archive_path, mustWork = FALSE)

  zip_result <- tryCatch(
    {
      utils::zip(
        zipfile = archive_abs,
        files = rel_files,
        flags = "-q"
      )
      0L
    },
    error = function(e) {
      warning(sprintf("ZIP failed: %s", conditionMessage(e)), call. = FALSE)
      1L
    }
  )

  archive_ok <- (zip_result == 0L) && file.exists(archive_abs)

  dplyr::tibble(
    archive_path = archive_path,
    n_files = length(files),
    archive_size_mb = if (archive_ok) {
      round(as.numeric(fs::file_size(archive_abs)) / 1024^2, 2)
    } else {
      NA_real_
    },
    success = archive_ok
  )
}


# ============================================================================
# S4  Manifest Parsing
# ============================================================================


# ----------------------------------------------------------------------------
# flatten_manifest
# ----------------------------------------------------------------------------
flatten_manifest <- function(manifest) {

  coalesce_null <- function(x, default) {
    if (is.null(x)) default else x
  }

  build_row <- function(source_block, file_entry, url_override = NULL) {
    dplyr::tibble(
      source_label = source_block$source_label,
      filename = file_entry$filename,
      url = if (!is.null(url_override)) url_override else file_entry$url,
      sha256 = coalesce_null(file_entry$sha256, NA_character_),
      size_mb = coalesce_null(file_entry$size_mb, NA_real_),
      active = coalesce_null(file_entry$active, FALSE),
      description = coalesce_null(file_entry$description, NA_character_),
      recorded_on = source_block$recorded_on,
      citation = source_block$citation,
      landing_url = source_block$landing_url
    )
  }

  rows_encovi <- purrr::map_dfr(
    manifest$encovi_2023$files,
    function(f) build_row(manifest$encovi_2023, f)
  )

  rows_sivesnu_2018 <- purrr::map_dfr(
    manifest$sivesnu$files_2018,
    function(f) build_row(manifest$sivesnu, f,
                          url_override = manifest$sivesnu$archive_url)
  )

  rows_nacimientos <- purrr::map_dfr(
    manifest$ine_nacimientos$files,
    function(f) build_row(manifest$ine_nacimientos, f)
  )

  rows_who <- purrr::map_dfr(
    manifest$who_growth$files,
    function(f) build_row(manifest$who_growth, f)
  )

  dplyr::bind_rows(
    rows_encovi,
    rows_sivesnu_2018,
    rows_nacimientos,
    rows_who
  )
}


# ============================================================================
# S5  Format Conversion & Fidelity Verification
# ============================================================================


# ----------------------------------------------------------------------------
# convert_one_file
# ----------------------------------------------------------------------------
convert_one_file <- function(original_path, parquet_path, source_label) {

  ext <- tolower(tools::file_ext(original_path))

  df <- switch(ext,
               "xlsx" = readxl::read_excel(original_path),
               "dta"  = haven::read_dta(original_path),
               stop(sprintf("Unsupported format: .%s", ext))
  )

  arrow::write_parquet(df, parquet_path)

  dplyr::tibble(
    source_label = source_label,
    original_filename = basename(original_path),
    parquet_filename = basename(parquet_path),
    original_nrow = nrow(df),
    original_ncol = ncol(df),
    conversion_success = file.exists(parquet_path)
  )
}


# ----------------------------------------------------------------------------
# convert_batch
# ----------------------------------------------------------------------------
convert_batch <- function(manifest_df) {

  to_convert <- manifest_df |> dplyr::filter(needs_conversion)

  if (nrow(to_convert) == 0) {
    return(dplyr::tibble(
      source_label = character(), original_filename = character(),
      parquet_filename = character(), original_nrow = integer(),
      original_ncol = integer(), conversion_success = logical()
    ))
  }

  results <- purrr::pmap_dfr(
    to_convert |> dplyr::select(original_path, parquet_path, source_label),
    function(original_path, parquet_path, source_label) {
      convert_one_file(original_path, parquet_path, source_label)
    }
  )

  results
}


# ----------------------------------------------------------------------------
# verify_one_pair
# ----------------------------------------------------------------------------
verify_one_pair <- function(original_path, parquet_path, source_label) {

  ext <- tolower(tools::file_ext(original_path))

  df_original <- switch(ext,
                        "xlsx" = readxl::read_excel(original_path),
                        "dta"  = haven::read_dta(original_path)
  )

  df_parquet <- arrow::read_parquet(parquet_path)

  dplyr::tibble(
    source_label = source_label,
    filename = basename(parquet_path),
    nrow_original = nrow(df_original),
    nrow_parquet = nrow(df_parquet),
    ncol_original = ncol(df_original),
    ncol_parquet = ncol(df_parquet),
    cols_match = identical(names(df_original), names(df_parquet)),
    rows_match = nrow(df_original) == nrow(df_parquet),
    fidelity_ok = identical(names(df_original), names(df_parquet)) &
      nrow(df_original) == nrow(df_parquet)
  )
}


# ----------------------------------------------------------------------------
# verify_batch
# ----------------------------------------------------------------------------
verify_batch <- function(manifest_df) {

  verifiable <- manifest_df |>
    dplyr::filter(file.exists(original_path), file.exists(parquet_path))

  if (nrow(verifiable) == 0) {
    return(dplyr::tibble(
      source_label = character(), filename = character(),
      nrow_original = integer(), nrow_parquet = integer(),
      ncol_original = integer(), ncol_parquet = integer(),
      cols_match = logical(), rows_match = logical(),
      fidelity_ok = logical()
    ))
  }

  results <- purrr::pmap_dfr(
    verifiable |> dplyr::select(original_path, parquet_path, source_label),
    function(original_path, parquet_path, source_label) {
      verify_one_pair(original_path, parquet_path, source_label)
    }
  )

  results
}


# ============================================================================
# S6  Reference Data Unification
# ============================================================================


# ----------------------------------------------------------------------------
# load_ine_birth_year
# ----------------------------------------------------------------------------
load_ine_birth_year <- function(anio, filename, sheet_name, skip_rows,
                                source_dir, departamentos_gt) {

  df <- readxl::read_excel(
    file.path(source_dir, filename),
    sheet = sheet_name,
    skip = skip_rows,
    col_names = c("mes_texto", "departamento_raw",
                  "total", "hombres", "mujeres")
  ) |>
    dplyr::filter(
      !is.na(mes_texto),
      !is.na(departamento_raw),
      !stringr::str_detect(departamento_raw,
                           "(?i)^total|^rep[u\u00fa]blica")
    ) |>
    dplyr::mutate(
      departamento = stringr::str_trim(departamento_raw),
      departamento = stringr::str_remove_all(departamento,
                                             "\\d+\\.?\\s*"),
      departamento = dplyr::case_when(
        stringr::str_detect(departamento, "(?i)progreso")     ~ "El Progreso",
        stringr::str_detect(departamento, "(?i)sacatep")       ~ "Sacatepequez",
        stringr::str_detect(departamento, "(?i)quich")         ~ "Quiche",
        stringr::str_detect(departamento, "(?i)solol")         ~ "Solola",
        stringr::str_detect(departamento, "(?i)totonicap")     ~ "Totonicapan",
        stringr::str_detect(departamento, "(?i)suchitep")      ~ "Suchitepequez",
        stringr::str_detect(departamento, "(?i)pet[e\u00e9]n") ~ "Peten",
        stringr::str_detect(departamento, "(?i)santa\\s*rosa")  ~ "Santa Rosa",
        stringr::str_detect(departamento, "(?i)san\\s*marcos")  ~ "San Marcos",
        stringr::str_detect(departamento, "(?i)baja\\s*verap")  ~ "Baja Verapaz",
        stringr::str_detect(departamento, "(?i)alta\\s*verap")  ~ "Alta Verapaz",
        TRUE ~ departamento
      ),
      mes = match(
        stringr::str_to_title(stringr::str_trim(mes_texto)),
        c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
          "Julio", "Agosto", "Septiembre", "Octubre",
          "Noviembre", "Diciembre")
      ),
      dplyr::across(
        c(total, hombres, mujeres),
        ~ suppressWarnings(as.integer(.x))
      ),
      anio = anio
    ) |>
    dplyr::filter(
      departamento %in% departamentos_gt,
      !is.na(mes)
    ) |>
    dplyr::select(anio, mes, departamento, total, hombres, mujeres)

  df
}


# ============================================================================
# S7  Cleanup
# ============================================================================


# ----------------------------------------------------------------------------
# cleanup_tmp_download
# ----------------------------------------------------------------------------
cleanup_tmp_download <- function(tmp_dir = NULL) {

  if (is.null(tmp_dir)) {
    tmp_dir <- here::here("01_data", "_tmp_download")
  }

  if (!fs::dir_exists(tmp_dir)) {
    return(invisible(TRUE))
  }

  tryCatch(
    {
      fs::dir_delete(tmp_dir)
      invisible(TRUE)
    },
    error = function(e) {
      warning(sprintf("Could not remove temporary directory: %s",
                      conditionMessage(e)), call. = FALSE)
      invisible(FALSE)
    }
  )
}
