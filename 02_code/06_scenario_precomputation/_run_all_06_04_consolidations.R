# ==============================================================================
# Local Orchestrator (mirai backend + Telegram notifications):
# Consolidations and Overview Precomputation (Step 06_04)
# ==============================================================================
#
# Parallel orchestrator over pathway variants. Dispatches each pathway
# to a persistent mirai daemon so that both variants render
# concurrently. Posts start/end notifications to a Telegram group for
# unattended monitoring.
#
# Outputs per variant:
#   - 06_04_{pathway}_departmental_scenarios_consolidated.parquet
#   - 06_04_{pathway}_national_scenarios_consolidated.parquet
#   - 06_04_{pathway}_overview_table.parquet
#
# Prerequisites:
#   - 06_02 outputs: departmental scenarios per (seed, subsidy, pathway) combination
#                    (9 parquets per pathway: 3 seeds x 3 subsidies)
#   - 06_03 outputs: national scenarios per (seed, subsidy, pathway) combination
#                    (9 parquets per pathway: 3 seeds x 3 subsidies)
#
# The execution grid is derived from the pathway_variants.yml config
# file alone. Unlike 06_02 and 06_03, which iterate over seed x subsidy
# x pathway, 06_04 consolidates results across all 9 (seed, subsidy)
# combinations for a given pathway and only iterates over pathway.
#
# Telegram notifications:
#   - Reads TELEGRAM_BOT_TOKEN and TELEGRAM_CHAT_ID from .Renviron.
#   - Posts a message at pipeline start and at pipeline end (with summary).
#   - Notification failures are silent: the pipeline never aborts because
#     of a Telegram issue.
#
# Design principles:
#   - Single source of truth: pathways live in pathway_variants.yml.
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
#   - Self-contained worker function: safe_render_combo() contains
#     all rendering and validation logic inline, with no free references
#     to other user-defined functions.
#   - Constant arguments passed via .args: mirai_map() uses .args (not
#     ...) for formal arguments of the worker function that should be
#     held constant across all map iterations.
#   - Persistent daemons: a pool of mirai daemons is spun up once at the
#     start of the run and torn down at the end.
#   - Robust iteration: every variant runs inside tryCatch inside its
#     daemon. A single variant failure does not abort the batch.
#   - Error timestamps: variant_start is captured before the tryCatch
#     so that failed variants report their real elapsed time.
#   - Atomic cumulative audit trail: log writes go to a temporary file
#     and are renamed in place, avoiding Windows memory-map collisions
#     and leaving the previous log intact on interruption.
#
# Usage:
#   source("02_code/06_scenario_precomputation/_run_all_06_04_consolidations.R")
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

safe_render_combo <- function(combo, qmd_input, parquet_output_dir) {

  # Captured before the tryCatch so failing combinations still report
  # their real elapsed time.
  combo_start <- Sys.time()

  tryCatch(
    {
      # --- Build a per-daemon unique copy of the .qmd -------------------

      qmd_dir  <- dirname(qmd_input)
      qmd_stem <- tools::file_path_sans_ext(basename(qmd_input))
      qmd_ext  <- tools::file_ext(basename(qmd_input))

      unique_tag <- paste0(
        combo$pathway, "_",
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

      # --- Render -------------------------------------------------------

      quarto::quarto_render(
        input          = qmd_local,
        execute_params = combo,
        quiet          = FALSE
      )

      combo_end <- Sys.time()

      # --- Post-execution validation ------------------------------------

      expected_parquets <- c(
        consolidated_dept = file.path(
          parquet_output_dir,
          paste0("06_04_", combo$pathway,
                 "_departmental_scenarios_consolidated.parquet")
        ),
        consolidated_nat = file.path(
          parquet_output_dir,
          paste0("06_04_", combo$pathway,
                 "_national_scenarios_consolidated.parquet")
        ),
        overview_table = file.path(
          parquet_output_dir,
          paste0("06_04_", combo$pathway, "_overview_table.parquet")
        )
      )

      parquet_checks <- purrr::map(expected_parquets, function(p) {
        exists <- file.exists(p)
        rows <- if (exists) {
          nrow(arrow::read_parquet(p, as_data_frame = FALSE))
        } else {
          NA_integer_
        }
        list(exists = exists, rows = rows)
      })

      for (nm in names(expected_parquets)) {
        if (!parquet_checks[[nm]]$exists) {
          stop("Expected parquet not generated: ", expected_parquets[[nm]])
        }
        if (isTRUE(parquet_checks[[nm]]$rows == 0)) {
          stop("Expected parquet is empty: ", expected_parquets[[nm]])
        }
      }

      tibble::tibble(
        pathway                  = combo$pathway,
        pathway_label            = combo$pathway_label,
        start_time               = combo_start,
        end_time                 = combo_end,
        duration_sec             = as.numeric(
          difftime(combo_end, combo_start, units = "secs")
        ),
        status                   = "success",
        error_message            = NA_character_,
        output_consolidated_dept = expected_parquets[["consolidated_dept"]],
        output_consolidated_nat  = expected_parquets[["consolidated_nat"]],
        output_overview_table    = expected_parquets[["overview_table"]],
        nrow_consolidated_dept   = parquet_checks[["consolidated_dept"]]$rows,
        nrow_consolidated_nat    = parquet_checks[["consolidated_nat"]]$rows,
        nrow_overview_table      = parquet_checks[["overview_table"]]$rows
      )
    },
    error = function(e) {
      combo_end <- Sys.time()
      tibble::tibble(
        pathway                  = if (is.null(combo$pathway))       NA_character_ else combo$pathway,
        pathway_label            = if (is.null(combo$pathway_label)) NA_character_ else combo$pathway_label,
        start_time               = combo_start,
        end_time                 = combo_end,
        duration_sec             = as.numeric(
          difftime(combo_end, combo_start, units = "secs")
        ),
        status                   = "error",
        error_message            = conditionMessage(e),
        output_consolidated_dept = NA_character_,
        output_consolidated_nat  = NA_character_,
        output_overview_table    = NA_character_,
        nrow_consolidated_dept   = NA_integer_,
        nrow_consolidated_nat    = NA_integer_,
        nrow_overview_table      = NA_integer_
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

  pathway_variants_path <- here("02_code", "_config", "pathway_variants.yml")

  if (!file.exists(pathway_variants_path)) {
    stop("Configuration file not found: ", pathway_variants_path)
  }

  pathway_variants <- yaml::read_yaml(pathway_variants_path)$variants

  if (length(pathway_variants) == 0) stop("No pathway variants found in ", pathway_variants_path)

  combos <- purrr::map(pathway_variants, function(pathway) {
    list(
      pathway       = pathway$pathway,
      pathway_label = pathway$pathway_label
    )
  })

  qmd_input <- here(
    "02_code", "06_scenario_precomputation",
    "06_04x_consolidate_and_overview.qmd"
  )

  parquet_output_dir <- here("01_data", "02_processed", "scenarios")

  log_dir <- here("01_data", "02_processed", "orchestrator_logs")
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }

  log_path <- file.path(log_dir, "06_04_consolidations_log.parquet")

  # --- Parallelism configuration --------------------------------------------

  # CPU reservation: cores left free for the OS, the main R process,
  # and other applications running on the host. M06 scenario scripts
  # carry heavier per-render workloads (large parquets, gt tables) than
  # M02, so we reserve more headroom. With only 2 pathway combinations,
  # this rarely matters, but the convention is preserved for consistency.
  cores_reserved <- 4

  physical_cores <- parallel::detectCores(logical = FALSE)
  n_daemons      <- min(length(combos), max(1, physical_cores - cores_reserved))

  mirai_profile  <- "orchestrator_06_04"

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

  run_id         <- format(Sys.time(), "%Y%m%d_%H%M%S")
  pipeline_start <- Sys.time()

  notify_telegram(sprintf(
    "*06_04 Consolidations* \u2014 pipeline started\nRun ID: `%s`\nCombinations: %d pathways\nDaemons: %d",
    run_id,
    length(combos),
    n_daemons
  ))

  run_rows <- mirai::mirai_map(
    .x       = combos,
    .f       = safe_render_combo,
    .args    = list(
      qmd_input          = qmd_input,
      parquet_output_dir = parquet_output_dir
    ),
    .compute = mirai_profile
  )[]

  run_df <- bind_rows(run_rows) |>
    mutate(
      run_id      = run_id,
      script_step = "06_04",
      .before     = 1
    )

  pipeline_end <- Sys.time()

  full_log <- append_to_log(run_df, log_path)

  summary_df <- run_df |>
    select(pathway, status, duration_sec,
           nrow_consolidated_dept, nrow_consolidated_nat, nrow_overview_table,
           error_message)

  n_success       <- sum(run_df$status == "success")
  n_failure       <- sum(run_df$status == "error")
  total_duration  <- round(as.numeric(
    difftime(pipeline_end, pipeline_start, units = "secs")
  ), 1)

  message("")
  message(strrep("=", 70))
  message("Consolidations Pipeline (mirai) \u2014 Execution Summary")
  message(strrep("=", 70))
  message("Run ID:            ", run_id)
  message(
    "Daemons used:      ", n_daemons, " / ",
    length(combos), " combinations (",
    length(pathway_variants), " pathways)"
  )
  message("Combinations run:  ", nrow(run_df))
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
    paste0(
      "*06_04 Consolidations* \u2014 pipeline finished\n",
      "Run ID: `%s`\n",
      "Successes: %d | Failures: %d\n",
      "Total duration: %.1f sec"
    ),
    run_id, n_success, n_failure,
    total_duration
  ))

  invisible(run_df)
}

# --- Launch -------------------------------------------------------------------

run_pipeline()
