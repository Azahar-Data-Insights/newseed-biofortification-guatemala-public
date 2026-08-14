# ==============================================================================
# Local Orchestrator (sequential + Telegram notifications):
# Module 05 - Height Transfer & Impact
# ==============================================================================
#
# Sequential orchestrator over the six scripts of module 05. Renders
# them one after the other, in the mandatory dependency order, because
# each script consumes outputs from the previous one. Posts start/end
# notifications to a Telegram group for unattended monitoring.
#
# Scope: scripts 05_01 through 05_06.
#   - 05_01  Prepare SIVESNU height data for transfer modeling
#   - 05_02  Train height transfer model (GAM)
#   - 05_03  Transfer height to ENCOVI (predict, quantile map, calibrate)
#   - 05_04  Calculate bioavailability inadequacy
#   - 05_05  Calculate QPM stunting impact
#   - 05_06  Assign biofortification priority
#
# Outputs per script (under 01_data/02_processed/):
#   - 05_01  transfer/05_01_sivesnu_height_modeling_data.parquet
#            models/05_01_yj_transforms.rds
#            models/05_01_boruta_results.rds
#   - 05_02  models/05_02_height_transfer_model.rds
#            transfer/05_02_cv_predictions.parquet
#   - 05_03  transfer/05_03_encovi_children_with_height.parquet
#            transfer/05_03_who_lms_reference.parquet
#   - 05_04  transfer/05_04_encovi_bioavailability_inadequacy.parquet
#   - 05_05  transfer/05_05_encovi_stunting_impact.parquet
#   - 05_06  transfer/05_06_encovi_children_priority.parquet
#
# Prerequisites:
#   - Module 01 outputs in 01_data/02_processed/transfer/
#     (01_04_individual_nutritional_intake_complete.parquet)
#   - SIVESNU 2018 raw data in 01_data/01_raw/SIVESNU_2018/
#   - ENCOVI 2023 raw data in 01_data/01_raw/ENCOVI_2023/
#   - External tables in 01_data/03_external/
#     (WHO growth standards, ENSMI stunting prevalence, EAR references)
#
# Telegram notifications:
#   - Reads TELEGRAM_BOT_TOKEN and TELEGRAM_CHAT_ID from .Renviron.
#   - Posts a message at pipeline start and at pipeline end (with summary).
#   - Notification failures are silent: the pipeline never aborts because
#     of a Telegram issue.
#
# Design principles:
#   - Sequential execution: scripts share a strict dependency chain
#     (05_01 -> 05_02 -> 05_03 -> 05_04 -> 05_05 -> 05_06). No
#     parallelization is applied at module level. Intra-script
#     parallelization is limited to multi-threaded ranger engines in
#     05_01 (impute_rf_parallel, Boruta) and 05_03 (impute_rf_parallel),
#     which use native thread-level parallelism within a single script.
#   - Function-scoped execution: the pipeline body runs inside
#     run_pipeline(). This gives on.exit() a proper function frame so
#     that teardown (Telegram end-notification, log flush) happens
#     exactly once, even if a script aborts.
#   - Self-contained worker function: safe_render_script() contains all
#     rendering and validation logic inline, with no free references to
#     other user-defined functions.
#   - Fail-fast semantics: because each script depends on the previous
#     one, a failure aborts the remaining steps. The log records the
#     failed script and the ones skipped as a consequence.
#   - Parquet-only outputs: framework orchestrators produce data files.
#     The HTML render is a quarto_render() by-product and is cleaned
#     up on exit together with .knit.md and _files/ artefacts. The
#     methodological website has its own rendering pipeline.
#   - Neutral CLI output: NO_COLOR is set before each render so Quarto
#     emits uncolored informational messages, which Positron and RStudio
#     display in the default text color.
#   - Error timestamps: script_start is captured before the tryCatch so
#     that failed scripts report their real elapsed time.
#   - Atomic cumulative audit trail: log writes go to a temporary file
#     and are renamed in place, avoiding Windows memory-map collisions
#     and leaving the previous log intact on interruption.
#
# Usage:
#   source("02_code/05_nutritional_impact_meta_analysis/_run_all_module05_height_impact.R")
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

# --- Worker function (executes one script) ------------------------------------

safe_render_script <- function(script_spec, qmd_input_dir) {

  # Captured before the tryCatch so failing scripts still report their
  # real elapsed time.
  script_start <- Sys.time()

  tryCatch(
    {
      qmd_input <- file.path(qmd_input_dir, script_spec$filename)

      if (!file.exists(qmd_input)) {
        stop("Quarto file not found: ", qmd_input)
      }

      # --- Schedule cleanup of rendering artefacts ----------------------
      #
      # These orchestrators produce parquets only. The HTML render is a
      # by-product of quarto_render() and must not be retained: the
      # methodological website has its own rendering pipeline and
      # framework outputs are restricted to data files. Cleanup runs on
      # exit so that artefacts are removed whether the render succeeds
      # or fails.

      qmd_dir  <- dirname(qmd_input)
      qmd_stem <- tools::file_path_sans_ext(basename(qmd_input))

      on.exit(
        {
          artefacts <- c(
            file.path(qmd_dir, paste0(qmd_stem, ".html")),
            file.path(qmd_dir, paste0(qmd_stem, ".knit.md")),
            file.path(qmd_dir, paste0(qmd_stem, "_files"))
          )
          unlink(artefacts, recursive = TRUE, force = TRUE)
        },
        add = TRUE
      )

      # --- Suppress ANSI color codes in Quarto CLI output ---------------
      #
      # Quarto CLI emits colored informational messages via stderr by
      # default. Positron and RStudio render the stderr stream in red,
      # which visually mimics an error even for successful runs. Setting
      # NO_COLOR disables ANSI escape sequences so messages appear in the
      # IDE's default text color.

      old_no_color <- Sys.getenv("NO_COLOR", unset = NA)
      Sys.setenv(NO_COLOR = "1")
      on.exit(
        {
          if (is.na(old_no_color)) {
            Sys.unsetenv("NO_COLOR")
          } else {
            Sys.setenv(NO_COLOR = old_no_color)
          }
        },
        add = TRUE
      )

      # --- Render -------------------------------------------------------

      quarto::quarto_render(
        input = qmd_input,
        quiet = FALSE
      )

      script_end <- Sys.time()

      # --- Post-execution validation ------------------------------------

      expected_outputs <- script_spec$expected_outputs
      outputs_exist    <- vapply(
        expected_outputs,
        file.exists,
        logical(1)
      )

      if (!all(outputs_exist)) {
        missing <- expected_outputs[!outputs_exist]
        stop(
          "Expected outputs not generated: ",
          paste(missing, collapse = ", ")
        )
      }

      tibble::tibble(
        script_step     = script_spec$step,
        script_filename = script_spec$filename,
        start_time      = script_start,
        end_time        = script_end,
        duration_sec    = as.numeric(
          difftime(script_end, script_start, units = "secs")
        ),
        status          = "success",
        error_message   = NA_character_,
        n_outputs       = length(expected_outputs),
        outputs_ok      = all(outputs_exist)
      )
    },
    error = function(e) {
      script_end <- Sys.time()
      tibble::tibble(
        script_step     = script_spec$step,
        script_filename = script_spec$filename,
        start_time      = script_start,
        end_time        = script_end,
        duration_sec    = as.numeric(
          difftime(script_end, script_start, units = "secs")
        ),
        status          = "error",
        error_message   = conditionMessage(e),
        n_outputs       = length(script_spec$expected_outputs),
        outputs_ok      = FALSE
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

  # --- Paths ----------------------------------------------------------------

  qmd_input_dir <- here("02_code", "05_nutritional_impact_meta_analysis")

  transfer_dir <- here("01_data", "02_processed", "transfer")
  models_dir   <- here("01_data", "02_processed", "models")

  log_dir <- here("01_data", "02_processed", "orchestrator_logs")
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }

  log_path <- file.path(log_dir, "module05_height_impact_log.parquet")

  # --- Script specifications ------------------------------------------------
  #
  # Each entry declares the step code, the .qmd filename, and the list of
  # expected output files. Post-execution validation checks that every
  # declared output exists on disk before marking the script as a success.
  #
  # Expected outputs include only data files (parquets and model RDS in
  # transfer/ and models/). Web resource RDS files are not validated by
  # the orchestrator — they are presentation artefacts, not pipeline data.

  script_specs <- list(
    list(
      step     = "05_01",
      filename = "05_01_prepare_sivesnu_height_data.qmd",
      expected_outputs = c(
        file.path(transfer_dir, "05_01_sivesnu_height_modeling_data.parquet"),
        file.path(models_dir,   "05_01_yj_transforms.rds"),
        file.path(models_dir,   "05_01_boruta_results.rds")
      )
    ),
    list(
      step     = "05_02",
      filename = "05_02_transfer_model_height.qmd",
      expected_outputs = c(
        file.path(models_dir,   "05_02_height_transfer_model.rds"),
        file.path(transfer_dir, "05_02_cv_predictions.parquet")
      )
    ),
    list(
      step     = "05_03",
      filename = "05_03_transfer_height_to_encovi.qmd",
      expected_outputs = c(
        file.path(transfer_dir, "05_03_encovi_children_with_height.parquet"),
        file.path(transfer_dir, "05_03_who_lms_reference.parquet")
      )
    ),
    list(
      step     = "05_04",
      filename = "05_04_calculate_bioavailability_inadequacy.qmd",
      expected_outputs = c(
        file.path(transfer_dir, "05_04_encovi_bioavailability_inadequacy.parquet")
      )
    ),
    list(
      step     = "05_05",
      filename = "05_05_calculate_qpm_stunting_impact.qmd",
      expected_outputs = c(
        file.path(transfer_dir, "05_05_encovi_stunting_impact.parquet")
      )
    ),
    list(
      step     = "05_06",
      filename = "05_06_assign_biofortification_priority.qmd",
      expected_outputs = c(
        file.path(transfer_dir, "05_06_encovi_children_priority.parquet")
      )
    )
  )

  # --- Execute --------------------------------------------------------------

  run_id         <- format(Sys.time(), "%Y%m%d_%H%M%S")
  pipeline_start <- Sys.time()

  notify_telegram(sprintf(
    "*Module 05 \u2014 Height Transfer & Impact* \u2014 started\nRun ID: `%s`\nScripts: %d (sequential)",
    run_id,
    length(script_specs)
  ))

  # Fail-fast sequential execution. accumulate() is preferred over walk()
  # here because we need to detect failures and stop dispatching the
  # remaining scripts, while still preserving the log rows produced so far.
  run_rows   <- vector("list", length(script_specs))
  stop_after <- length(script_specs)

  for (i in seq_along(script_specs)) {
    spec <- script_specs[[i]]
    message("")
    message(strrep("-", 70))
    message("Running ", spec$step, " : ", spec$filename)
    message(strrep("-", 70))

    result_row    <- safe_render_script(spec, qmd_input_dir)
    run_rows[[i]] <- result_row

    if (result_row$status == "error") {
      stop_after <- i
      break
    }
  }

  # Mark scripts that were skipped because an upstream failure aborted
  # the chain. Their dependencies were not produced, so they cannot run.
  if (stop_after < length(script_specs)) {
    for (i in (stop_after + 1):length(script_specs)) {
      spec          <- script_specs[[i]]
      skipped_time  <- Sys.time()
      run_rows[[i]] <- tibble::tibble(
        script_step     = spec$step,
        script_filename = spec$filename,
        start_time      = skipped_time,
        end_time        = skipped_time,
        duration_sec    = 0,
        status          = "skipped",
        error_message   = "Upstream failure in pipeline chain",
        n_outputs       = length(spec$expected_outputs),
        outputs_ok      = FALSE
      )
    }
  }

  run_df <- bind_rows(run_rows) |>
    mutate(
      run_id = run_id,
      module = "05_height_impact",
      .before = 1
    )

  pipeline_end <- Sys.time()

  full_log <- append_to_log(run_df, log_path)

  summary_df <- run_df |>
    select(script_step, status, duration_sec, n_outputs, outputs_ok, error_message)

  n_success       <- sum(run_df$status == "success")
  n_failure       <- sum(run_df$status == "error")
  n_skipped       <- sum(run_df$status == "skipped")
  total_duration  <- round(as.numeric(
    difftime(pipeline_end, pipeline_start, units = "secs")
  ), 1)

  message("")
  message(strrep("=", 70))
  message("Module 05 \u2014 Height Transfer & Impact (sequential) \u2014 Execution Summary")
  message(strrep("=", 70))
  message("Run ID:            ", run_id)
  message("Scripts declared:  ", length(script_specs))
  message(
    "Successes:         ", n_success,
    " | Failures: ",        n_failure,
    " | Skipped: ",          n_skipped
  )
  message("Total duration:    ", total_duration, " sec (wall clock)")
  message("Log written to:    ", log_path)
  message("Total runs logged: ", length(unique(full_log$run_id)))
  message(strrep("=", 70))

  print(summary_df)

  notify_telegram(sprintf(
    "*Module 05 \u2014 Height Transfer & Impact* \u2014 finished\nRun ID: `%s`\nSuccesses: %d | Failures: %d | Skipped: %d\nTotal duration: %.1f sec",
    run_id, n_success, n_failure, n_skipped, total_duration
  ))

  invisible(run_df)
}

# --- Launch -------------------------------------------------------------------

run_pipeline()
