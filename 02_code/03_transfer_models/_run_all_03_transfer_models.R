# ==============================================================================
# Local Orchestrator (mirai backend + Telegram notifications):
# Transfer Models by Nutrient (Step 03)
# ==============================================================================
#
# Parallel orchestrator over nutrient variants. Dispatches each
# nutrient to a persistent mirai daemon so that all variants render
# concurrently. Posts start/end notifications to a Telegram group for
# unattended monitoring.
#
# Scope: scripts 03_02 (iron) through 03_07 (energy). Script 03_08
# (plant percentage) is excluded because its modelling structure
# differs substantially from nutrient intake variants and is
# maintained as a separate non-parametrized script.
#
# Outputs per variant:
#   - {variant$model_filename}  under 01_data/02_processed/models/
#     (the filename is declared by the YAML config, not constructed
#     from the variant suffix)
#
# Prerequisites:
#   - 03_01 outputs: modeling-ready dataset
#     (01_data/02_processed/transfer/03_01_modeling_ready_data.parquet)
#
# Telegram notifications:
#   - Reads TELEGRAM_BOT_TOKEN and TELEGRAM_CHAT_ID from .Renviron.
#   - Posts a message at pipeline start and at pipeline end (with summary).
#   - Notification failures are silent: the pipeline never aborts because
#     of a Telegram issue.
#
# Design principles:
#   - Single source of truth: nutrients live in nutrient_variants.yml.
#     All fields are flat scalars (no nested structures), so the
#     variant list is passed directly to quarto_render() without
#     structural transformation.
#   - In-tree rendering with per-daemon unique names: the .qmd is
#     copied under a unique name into its own source directory. Each
#     daemon renders a distinct filename, so Quarto's intermediate
#     artefacts (.knit.md, .html, _files/) carry different names and
#     do not collide. Keeping the copy inside the project tree ensures
#     that here::here() inside the .qmd resolves the project root
#     correctly.
#   - Named mirai profile: a dedicated compute profile isolates this
#     orchestrator's daemon pool from any other mirai usage in the
#     session. Prevents collisions with other orchestrators and makes
#     setup/teardown explicit.
#   - Function-scoped execution: the pipeline body runs inside
#     run_pipeline(). This gives on.exit() a proper function frame so
#     that daemons are torn down exactly once, at the end of the run,
#     even if mirai_map() aborts.
#   - Self-contained worker function: safe_render_variant() contains
#     all rendering and validation logic inline, with no free references
#     to other user-defined functions.
#   - Constant arguments passed via .args: mirai_map() uses .args (not
#     ...) for formal arguments of the worker function that should be
#     held constant across all map iterations.
#   - Persistent daemons: a pool of mirai daemons is spun up once at the
#     start of the run and torn down at the end.
#   - Robust iteration: every variant runs inside tryCatch inside its
#     daemon. A single variant failure does not abort the batch.
#   - RDS validation: outputs are serialized model files, not parquets.
#     Validation checks existence and non-zero file size, not row count.
#   - Error timestamps: variant_start is captured before the tryCatch
#     so that failed variants report their real elapsed time.
#   - Atomic cumulative audit trail: log writes go to a temporary file
#     and are renamed in place, avoiding Windows memory-map collisions
#     and leaving the previous log intact on interruption.
#
# Usage:
#   source("02_code/03_transfer_models/_run_all_03_transfer_models.R")
#
# ==============================================================================

# --- Environment setup --------------------------------------------------------

rm(list = ls())
tryCatch(
  suppressWarnings(pacman::p_unload(all)),
  error = function(e) invisible(NULL)
)

pacman::p_load(
  arrow,          # Parquet read/write for the validation log
  here,           # Robust file paths
  httr2,          # Telegram API calls
  mirai,          # Persistent daemon pool for parallel execution
  quarto,         # Parametrized rendering
  RhpcBLASctl,    # BLAS/OpenMP thread control inside daemons
  yaml,           # Configuration file parsing
  tidyverse       # purrr, tibbles, pipes
)

select <- dplyr::select
filter <- dplyr::filter

# --- Telegram notification helpers -------------------------------------------

notify_telegram <- function(text) {
  token   <- Sys.getenv("TELEGRAM_BOT_TOKEN", unset = "")
  chat_id <- Sys.getenv("TELEGRAM_CHAT_ID",   unset = "")

  if (!nzchar(token) || !nzchar(chat_id)) {
    return(invisible(FALSE))
  }

  result <- tryCatch({
    httr2::request(paste0("https://api.telegram.org/bot", token, "/sendMessage")) |>
      httr2::req_body_json(list(
        chat_id    = chat_id,
        text       = text,
        parse_mode = "Markdown"
      )) |>
      httr2::req_timeout(10) |>
      httr2::req_perform()
    TRUE
  }, error = function(e) FALSE)

  invisible(result)
}

# --- Worker function (executes inside each mirai daemon) ----------------------

safe_render_variant <- function(variant, qmd_input, models_output_dir) {

  # Captured before the tryCatch so failing variants report their real
  # elapsed time.
  variant_start <- Sys.time()

  tryCatch(
    {
      # --- Build a per-daemon unique copy of the .qmd -------------------

      qmd_dir  <- dirname(qmd_input)
      qmd_stem <- tools::file_path_sans_ext(basename(qmd_input))
      qmd_ext  <- tools::file_ext(basename(qmd_input))

      unique_tag <- paste0(
        variant$nutrient_id, "_",
        format(Sys.time(), "%H%M%S"), "_",
        sprintf("%06x", sample.int(16777215, 1))
      )

      qmd_local_name <- paste0(qmd_stem, "__", unique_tag, ".", qmd_ext)
      qmd_local      <- file.path(qmd_dir, qmd_local_name)

      file.copy(qmd_input, qmd_local, overwrite = TRUE)

      on.exit({
        local_stem <- tools::file_path_sans_ext(qmd_local_name)
        candidates <- list.files(
          qmd_dir,
          pattern    = paste0("^", local_stem, "([._].*)?$"),
          full.names = TRUE
        )
        unlink(candidates, recursive = TRUE, force = TRUE)
      }, add = TRUE)

      # --- Flatten params -----------------------------------------------
      #
      # All fields in nutrient_variants.yml are flat scalars, matching
      # directly the parameters declared by 03_0x_transfer_model.qmd.
      # This list documents the fields propagated to the render.

      execute_params <- list(
        nutrient_id      = variant$nutrient_id,
        nutrient_label   = variant$nutrient_label,
        var_prefix       = variant$var_prefix,
        var_maize        = variant$var_maize,
        var_non_maize    = variant$var_non_maize,
        units            = variant$units,
        script_number    = variant$script_number,
        model_filename   = variant$model_filename,
        outlier_quantile = variant$outlier_quantile,
        outlier_label    = variant$outlier_label,
        distribution_fn  = variant$distribution_fn,
        overview_text    = variant$overview_text
      )

      # --- Render -------------------------------------------------------

      quarto::quarto_render(
        input          = qmd_local,
        execute_params = execute_params,
        quiet          = FALSE
      )

      variant_end <- Sys.time()

      # --- Post-execution validation ------------------------------------
      #
      # Outputs are RDS model files. The filename is declared by the
      # YAML config (variant$model_filename), not constructed here.
      # Validation checks existence and non-zero size; row counting
      # does not apply to serialized model objects.

      expected_rds <- file.path(models_output_dir, variant$model_filename)

      rds_exists <- file.exists(expected_rds)

      rds_size <- if (rds_exists) {
        file.size(expected_rds)
      } else {
        NA_integer_
      }

      if (!rds_exists) {
        stop("Expected RDS model file not generated: ", expected_rds)
      }

      if (isTRUE(rds_size == 0)) {
        stop("Expected RDS model file is empty: ", expected_rds)
      }

      tibble::tibble(
        nutrient_id    = variant$nutrient_id,
        script_number  = variant$script_number,
        start_time     = variant_start,
        end_time       = variant_end,
        duration_sec   = as.numeric(
          difftime(variant_end, variant_start, units = "secs")
        ),
        status         = "success",
        error_message  = NA_character_,
        output_rds     = expected_rds,
        output_exists  = rds_exists,
        output_size_kb = round(rds_size / 1024, 1)
      )
    },
    error = function(e) {
      variant_end <- Sys.time()
      tibble::tibble(
        nutrient_id    = if (is.null(variant$nutrient_id))   NA_character_ else variant$nutrient_id,
        script_number  = if (is.null(variant$script_number)) NA_character_ else variant$script_number,
        start_time     = variant_start,
        end_time       = variant_end,
        duration_sec   = as.numeric(
          difftime(variant_end, variant_start, units = "secs")
        ),
        status         = "error",
        error_message  = conditionMessage(e),
        output_rds     = NA_character_,
        output_exists  = FALSE,
        output_size_kb = NA_real_
      )
    }
  )
}

# --- Append to cumulative log -------------------------------------------------

append_to_log <- function(new_rows, log_path) {
  if (file.exists(log_path)) {
    previous <- arrow::read_parquet(log_path) |>
      tibble::as_tibble()
    gc(verbose = FALSE)
    combined <- bind_rows(previous, new_rows)
  } else {
    combined <- new_rows
  }

  tmp_path <- paste0(log_path, ".tmp")
  arrow::write_parquet(combined, tmp_path)

  if (file.exists(log_path)) {
    file.remove(log_path)
  }
  file.rename(tmp_path, log_path)

  combined
}

# --- Pipeline body ------------------------------------------------------------

run_pipeline <- function() {

  nutrient_variants_path <- here("02_code", "_config", "nutrient_variants.yml")

  if (!file.exists(nutrient_variants_path)) {
    stop("Configuration file not found: ", nutrient_variants_path)
  }

  nutrient_variants <- yaml::read_yaml(nutrient_variants_path)$variants

  if (length(nutrient_variants) == 0) {
    stop("No variants found in ", nutrient_variants_path)
  }

  # --- Paths ----------------------------------------------------------------

  qmd_input <- here(
    "02_code", "03_transfer_models",
    "03_0x_transfer_model.qmd"
  )

  models_output_dir <- here("01_data", "02_processed", "models")

  log_dir <- here("01_data", "02_processed", "orchestrator_logs")
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }

  log_path <- file.path(log_dir, "03_transfer_models_log.parquet")

  # --- Parallelism configuration --------------------------------------------

  physical_cores <- parallel::detectCores(logical = FALSE)
  n_daemons      <- min(length(nutrient_variants), max(1, physical_cores - 4))

  mirai_profile  <- "orchestrator_03"

  # --- Spin up daemons ------------------------------------------------------

  mirai::daemons(n_daemons, .compute = mirai_profile)

  on.exit(mirai::daemons(0, .compute = mirai_profile), add = TRUE)

  mirai::everywhere(
    {
      suppressPackageStartupMessages({
        library(quarto)
        library(arrow)
        library(here)
        library(tibble)
      })
      if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
        RhpcBLASctl::blas_set_num_threads(1)
        RhpcBLASctl::omp_set_num_threads(1)
      }
      # Suppress ANSI color codes in Quarto CLI output, so the progress
      # messages Quarto emits through stderr appear in the IDE's default
      # text color.
      Sys.setenv(NO_COLOR = "1")
    },
    .compute = mirai_profile
  )

  # --- Execute --------------------------------------------------------------

  run_id         <- format(Sys.time(), "%Y%m%d_%H%M%S")
  pipeline_start <- Sys.time()

  notify_telegram(sprintf(
    "*03 Transfer Models* — pipeline started\nRun ID: `%s`\nVariants: %d nutrients\nDaemons: %d",
    run_id,
    length(nutrient_variants),
    n_daemons
  ))

  run_rows <- mirai::mirai_map(
    .x       = nutrient_variants,
    .f       = safe_render_variant,
    .args    = list(
      qmd_input         = qmd_input,
      models_output_dir = models_output_dir
    ),
    .compute = mirai_profile
  )[]

  run_df <- bind_rows(run_rows) |>
    mutate(
      run_id      = run_id,
      script_step = "03",
      .before     = 1
    )

  pipeline_end <- Sys.time()

  full_log <- append_to_log(run_df, log_path)

  summary_df <- run_df |>
    select(script_number, nutrient_id, status, duration_sec,
           output_size_kb, error_message)

  n_success       <- sum(run_df$status == "success")
  n_failure       <- sum(run_df$status == "error")
  total_duration  <- round(as.numeric(
    difftime(pipeline_end, pipeline_start, units = "secs")
  ), 1)

  message("")
  message(strrep("=", 70))
  message("Transfer Models Pipeline (mirai) — Execution Summary")
  message(strrep("=", 70))
  message("Run ID:            ", run_id)
  message("Daemons used:      ", n_daemons, " / ", length(nutrient_variants), " variants")
  message("Variants run:      ", nrow(run_df))
  message(
    "Successes:         ", n_success,
    " | Failures: ",        n_failure
  )
  message("Total duration:    ", total_duration, " sec (wall clock)")
  message("Log written to:    ", log_path)
  message("Total runs logged: ", length(unique(full_log$run_id)))
  message(strrep("=", 70))

  print(summary_df)

  notify_telegram(sprintf(
    "*03 Transfer Models* — pipeline finished\nRun ID: `%s`\nSuccesses: %d | Failures: %d\nTotal duration: %.1f sec",
    run_id, n_success, n_failure, total_duration
  ))

  invisible(run_df)
}

# --- Launch -------------------------------------------------------------------

run_pipeline()
