# ==============================================================================
# Local Orchestrator (sequential + Telegram notifications):
# Module 00 - Data Provisioning
# ==============================================================================
#
# Sequential orchestrator for one-time data provisioning. Executes the
# three provisioning scripts in strict dependency order:
#
#   1. 00_01  Download and verify raw data sources
#   2. 00_02  Convert originals to Parquet, verify fidelity, archive, cleanup
#   3. 06_00  Compute OSRM departmental travel time matrix
#
# Scripts 00_01 and 00_02 provision the 01_data/01_raw/ directory with
# Parquet files ready for the analytical pipeline. Script 06_00 generates
# the travel time matrix consumed by the gravity redistribution model in
# Module 06; it has no dependency on framework data and is included here
# because it is a one-time infrastructure computation that does not belong
# in the recurrent Module 06 orchestrators.
#
# This orchestrator is intended for auditors and new environment setup.
# An environment that already holds the provisioned data can skip it;
# re-running this orchestrator re-downloads from public URLs.
#
# Telegram notifications:
#   - Reads TELEGRAM_BOT_TOKEN and TELEGRAM_CHAT_ID from .Renviron.
#   - Posts a message at pipeline start and at pipeline end.
#   - Notification failures are silent.
#
# Usage:
#   source("02_code/00_data_provisioning/_run_all_module00_provisioning.R")
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
  quarto,         # Quarto rendering
  tidyverse       # purrr, tibbles, pipes
)

select <- dplyr::select
filter <- dplyr::filter

# --- Shared helpers (notify_telegram, append_to_log, safe_render_step,
# ---  build_combos) -----------------------------------------------------------

source(here("02_code", "00_functions", "_helpers_orchestrators.R"))

# --- Pipeline body ------------------------------------------------------------

run_pipeline <- function() {

  # --- Paths ----------------------------------------------------------------
  code_dir_00 <- here("02_code", "00_data_provisioning")
  code_dir_06 <- here("02_code", "06_scenarios")

  log_dir <- here("01_data", "02_processed", "orchestrator_logs")
  if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)

  log_path <- file.path(log_dir, "module00_provisioning_log.parquet")

  # --- Output directories (created by the scripts themselves, but
  # --- declared here for expected_outputs validation) -----------------------

  raw_root     <- here("01_data", "01_raw")
  transfer_dir <- here("01_data", "02_processed", "transfer")
  external_dir <- here("01_data", "02_processed", "external")

  # --- Step specifications --------------------------------------------------
  #
  # Each entry declares:
  #   - step             : step code for the log
  #   - filename         : .qmd file to render
  #   - type             : "qmd" (all three are direct renders)
  #   - code_dir         : directory containing the script
  #   - expected_outputs : files that must exist after execution

  step_specs <- list(
    list(
      step     = "00_01",
      filename = "00_01_download_raw_sources.qmd",
      type     = "qmd",
      code_dir = code_dir_00,
      expected_outputs = c(
        file.path(raw_root, "_hashes_active.csv")
      )
    ),
    list(
      step     = "00_02",
      filename = "00_02_convert_to_parquet.qmd",
      type     = "qmd",
      code_dir = code_dir_00,
      expected_outputs = c(
        file.path(raw_root, "_originals", "active_raw_sources.zip"),
        file.path(raw_root, "INE_NACIMIENTOS",
                  "ine_nacimientos_2019_2023.parquet"),
        file.path(raw_root, "WHO_GROWTH",
                  "who_standards_lhfa_monthly.parquet"),
        file.path(raw_root, "WHO_GROWTH",
                  "who_standards_lhfa_daily.parquet"),
        file.path(raw_root, "WHO_GROWTH",
                  "who_reference_hfa_monthly.parquet")
      )
    ),
    list(
      step     = "06_00",
      filename = "06_00_compute_travel_time_matrix.qmd",
      type     = "qmd",
      code_dir = code_dir_06,
      expected_outputs = c(
        file.path(external_dir, "06_00_osrm_travel_time_matrix.parquet"),
        file.path(external_dir, "06_00_departmental_capitals.parquet")
      )
    )
  )

  # --- Execute --------------------------------------------------------------

  run_id         <- format(Sys.time(), "%Y%m%d_%H%M%S")
  pipeline_start <- Sys.time()

  notify_telegram(sprintf(
    "*Module 00 \u2014 Data Provisioning* \u2014 started\nRun ID: `%s`\nSteps: %d (sequential)",
    run_id, length(step_specs)
  ))

  # Fail-fast sequential execution. A step failure aborts the chain.
  run_rows   <- vector("list", length(step_specs))
  stop_after <- length(step_specs)

  for (i in seq_along(step_specs)) {
    spec <- step_specs[[i]]
    message("")
    message(strrep("-", 70))
    message("Running ", spec$step, " [", spec$type, "] : ", spec$filename)
    message(strrep("-", 70))

    # safe_render_step expects code_dir as second argument; here each
    # step may live in a different directory, so we override per step
    result_row    <- safe_render_step(spec, spec$code_dir)
    run_rows[[i]] <- result_row

    if (result_row$status == "error") {
      stop_after <- i
      break
    }
  }

  # Mark steps skipped due to upstream failure.
  if (stop_after < length(step_specs)) {
    for (i in (stop_after + 1):length(step_specs)) {
      spec          <- step_specs[[i]]
      skipped_time  <- Sys.time()
      run_rows[[i]] <- tibble::tibble(
        script_step   = spec$step,
        step_type     = spec$type,
        step_filename = spec$filename,
        start_time    = skipped_time,
        end_time      = skipped_time,
        duration_sec  = 0,
        status        = "skipped",
        error_message = "Upstream failure in pipeline chain",
        n_outputs     = length(spec$expected_outputs),
        outputs_ok    = FALSE
      )
    }
  }

  run_df <- bind_rows(run_rows) |>
    mutate(run_id = run_id, module = "00_provisioning", .before = 1)

  pipeline_end   <- Sys.time()
  full_log       <- append_to_log(run_df, log_path)
  total_duration <- round(as.numeric(
    difftime(pipeline_end, pipeline_start, units = "secs")
  ), 1)

  n_success <- sum(run_df$status == "success")
  n_failure <- sum(run_df$status == "error")
  n_skipped <- sum(run_df$status == "skipped")

  message("")
  message(strrep("=", 70))
  message("Module 00 \u2014 Data Provisioning \u2014 Execution Summary")
  message(strrep("=", 70))
  message("Run ID:            ", run_id)
  message("Steps declared:    ", length(step_specs))
  message("Successes: ", n_success, " | Failures: ", n_failure,
          " | Skipped: ", n_skipped)
  message("Total duration:    ", total_duration, " sec (wall clock)")
  message("Log written to:    ", log_path)
  message("Total runs logged: ", length(unique(full_log$run_id)))
  message(strrep("=", 70))

  print(run_df |> select(script_step, step_type, status, duration_sec, outputs_ok))

  notify_telegram(sprintf(
    "*Module 00 \u2014 Data Provisioning* \u2014 finished\nRun ID: `%s`\nSuccesses: %d | Failures: %d | Skipped: %d\nTotal duration: %.1f sec",
    run_id, n_success, n_failure, n_skipped, total_duration
  ))

  invisible(run_df)
}

# --- Launch -------------------------------------------------------------------

run_pipeline()
