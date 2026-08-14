# ==============================================================================
# Local Orchestrator (sequential + Telegram notifications):
# Module 03 - Transfer Models
# ==============================================================================
#
# Sequential orchestrator over the three steps of module 03. Executes the
# preparation script (03_01), dispatches the nested parametric
# sub-orchestrator (03_0x) that handles the nutrient variants, and finally
# runs the closing plant percentage script (03_08).
# Posts start/end notifications to a Telegram group for unattended
# monitoring.
#
# Scope: three pipeline steps executed in dependency order.
#   - 03_01  Prepare modeling data                         (direct .qmd)
#   - 03_0x  Transfer models by nutrient                   (sub-orchestrator)
#   - 03_08  Transfer model for plant percentage           (direct .qmd)
#
# Outputs per step (under 01_data/02_processed/):
#   - 03_01  transfer/03_01_modeling_ready_data.parquet
#   - 03_0x  models/{model_filename} for each nutrient variant
#            (filenames are declared by nutrient_variants.yml)
#   - 03_08  models/03_08_plant_percentage_model.rds
#
# Prerequisites:
#   - Module 01 outputs available in 01_data/02_processed/transfer/
#     (01_04_individual_nutritional_intake_complete.parquet)
#   - 02_code/_config/nutrient_variants.yml defined for the parametric step
#
# Telegram notifications:
#   - Reads TELEGRAM_BOT_TOKEN and TELEGRAM_CHAT_ID from .Renviron.
#   - Posts a message at pipeline start and at pipeline end (with summary).
#   - Nested sub-orchestrator (03_0x) emits its own start/end
#     notifications, giving step-level visibility.
#   - Notification failures are silent: the pipeline never aborts because
#     of a Telegram issue.
#
# Design principles:
#   - Sequential execution: steps share a strict dependency chain
#     (03_01 -> 03_0x -> 03_08). No parallelization is applied at module
#     level; parallelism is encapsulated inside the parametric
#     sub-orchestrator.
#   - Sub-orchestrator invocation via isolated source: the parametric
#     sub-orchestrator is invoked with source(path, local = TRUE) into
#     a fresh environment, so its internal helpers (notify_telegram,
#     safe_render_variant, append_to_log) do not contaminate the module
#     orchestrator's namespace.
#   - Function-scoped execution: the pipeline body runs inside
#     run_pipeline(). This gives on.exit() a proper function frame so
#     that teardown (Telegram end-notification, log flush) happens
#     exactly once, even if a step aborts.
#   - Self-contained worker function: safe_render_step() contains all
#     rendering, sub-orchestrator invocation, and validation logic
#     inline, with no free references to other user-defined functions.
#   - Fail-fast semantics: because each step depends on the previous
#     one, a failure aborts the remaining steps. The log records the
#     failed step and the ones skipped as a consequence. For the
#     parametric sub-orchestrator, a partial failure (any nutrient
#     variant missing its output .rds) is treated as a step failure.
#   - Step-level timing granularity: the module log captures start_time,
#     end_time, and duration_sec per step as a single block. Sub-step
#     timing (per nutrient variant for the parametric step) lives in
#     the sub-orchestrator's own log, queryable independently.
#   - Data-output-only validation: expected_outputs cover final data
#     deliverables only (parquets of processed data, RDS of fitted
#     models). Intermediate .rds files under web_resources/ are not
#     validated here: they are artefacts of the methodological website,
#     not pipeline outputs.
#   - HTML artefact cleanup: the HTML render is a quarto_render()
#     by-product and is cleaned up on exit together with .knit.md and
#     _files/. Applies only to direct .qmd steps; the sub-orchestrator
#     cleans its own artefacts internally.
#   - Neutral CLI output: NO_COLOR is set before each render so Quarto
#     emits uncolored informational messages, which Positron and RStudio
#     display in the default text color.
#   - Error timestamps: step_start is captured before the tryCatch so
#     that failed steps report their real elapsed time.
#   - Atomic cumulative audit trail: log writes go to a temporary file
#     and are renamed in place, avoiding Windows memory-map collisions
#     and leaving the previous log intact on interruption.
#
# Usage:
#   source("02_code/03_transfer_models/_run_all_module03_transfer_models.R")
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

# --- Worker function (executes one step) --------------------------------------

safe_render_step <- function(step_spec, code_dir) {

  # Captured before the tryCatch so failing steps report their real
  # elapsed time.
  step_start <- Sys.time()

  tryCatch(
    {
      step_input <- file.path(code_dir, step_spec$filename)

      if (!file.exists(step_input)) {
        stop("Step file not found: ", step_input)
      }

      # --- Dispatch on step type ----------------------------------------
      #
      # Two execution paths: direct Quarto rendering for preparation
      # and closing scripts, or sourced sub-orchestrator for the
      # parametric step.

      if (identical(step_spec$type, "qmd")) {

        # --- Schedule cleanup of rendering artefacts --------------------

        qmd_dir  <- dirname(step_input)
        qmd_stem <- tools::file_path_sans_ext(basename(step_input))

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

        # --- Suppress ANSI color codes in Quarto CLI output -------------

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

        # --- Render -----------------------------------------------------

        quarto::quarto_render(
          input = step_input,
          quiet = FALSE
        )

      } else if (identical(step_spec$type, "suborchestrator")) {

        # --- Invoke nested parametric sub-orchestrator ------------------
        #
        # source(local = TRUE) isolates the sub-orchestrator's internal
        # helpers from this function's namespace. The sub-orchestrator
        # manages its own daemons, rendering artefacts cleanup, and
        # log. Its final notify_telegram() call is emitted as part of
        # the natural step duplication described in the header.

        suborchestrator_env <- new.env(parent = globalenv())
        sys.source(step_input, envir = suborchestrator_env)

      } else {
        stop("Unknown step type: ", step_spec$type,
             " (expected 'qmd' or 'suborchestrator')")
      }

      step_end <- Sys.time()

      # --- Post-execution validation ------------------------------------

      expected_outputs <- step_spec$expected_outputs
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
        script_step   = step_spec$step,
        step_type     = step_spec$type,
        step_filename = step_spec$filename,
        start_time    = step_start,
        end_time      = step_end,
        duration_sec  = as.numeric(
          difftime(step_end, step_start, units = "secs")
        ),
        status        = "success",
        error_message = NA_character_,
        n_outputs     = length(expected_outputs),
        outputs_ok    = all(outputs_exist)
      )
    },
    error = function(e) {
      step_end <- Sys.time()
      tibble::tibble(
        script_step   = step_spec$step,
        step_type     = step_spec$type,
        step_filename = step_spec$filename,
        start_time    = step_start,
        end_time      = step_end,
        duration_sec  = as.numeric(
          difftime(step_end, step_start, units = "secs")
        ),
        status        = "error",
        error_message = conditionMessage(e),
        n_outputs     = length(step_spec$expected_outputs),
        outputs_ok    = FALSE
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

  code_dir <- here("02_code", "03_transfer_models")

  transfer_dir <- here("01_data", "02_processed", "transfer")
  models_dir   <- here("01_data", "02_processed", "models")

  log_dir <- here("01_data", "02_processed", "orchestrator_logs")
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }

  log_path <- file.path(log_dir, "module03_transfer_models_log.parquet")

  # --- Nutrient variants (used to enumerate expected outputs for the
  # parametric step) ---------------------------------------------------------
  #
  # The parametric step 03_0x renders one .rds model file per nutrient
  # variant. Filenames are declared directly in the YAML config
  # (field: model_filename), not constructed from the variant id.

  nutrient_variants_path <- here("02_code", "_config", "nutrient_variants.yml")
  if (!file.exists(nutrient_variants_path)) {
    stop("Configuration file not found: ", nutrient_variants_path)
  }

  nutrient_model_filenames <- yaml::read_yaml(nutrient_variants_path)$variants |>
    purrr::map_chr("model_filename")

  # --- Step specifications --------------------------------------------------
  #
  # Each entry declares the step code, the file to execute, the type
  # ('qmd' for direct Quarto render, 'suborchestrator' for the nested
  # parametric pipeline), and the list of expected output files.
  # Post-execution validation checks that every declared output
  # exists on disk before marking the step as a success.
  #
  # Outputs validated here are final data deliverables only (parquets
  # of processed data, RDS of fitted models). Intermediate .rds files
  # under web_resources/ are not validated at module level.

  step_specs <- list(
    list(
      step     = "03_01",
      filename = "03_01_prepare_modeling_data.qmd",
      type     = "qmd",
      expected_outputs = c(
        file.path(transfer_dir, "03_01_modeling_ready_data.parquet")
      )
    ),
    list(
      step     = "03_0x",
      filename = "_run_all_03_transfer_models.R",
      type     = "suborchestrator",
      expected_outputs = file.path(models_dir, nutrient_model_filenames)
    ),
    list(
      step     = "03_08",
      filename = "03_08_transfer_model_plant_percentage.qmd",
      type     = "qmd",
      expected_outputs = c(
        file.path(models_dir, "03_08_plant_percentage_model.rds")
      )
    )
  )

  # --- Execute --------------------------------------------------------------

  run_id         <- format(Sys.time(), "%Y%m%d_%H%M%S")
  pipeline_start <- Sys.time()

  notify_telegram(sprintf(
    "*Module 03 \u2014 Transfer Models* \u2014 started\nRun ID: `%s`\nSteps: %d (sequential)",
    run_id,
    length(step_specs)
  ))

  # Fail-fast sequential execution. accumulate() is preferred over walk()
  # here because we need to detect failures and stop dispatching the
  # remaining steps, while still preserving the log rows produced so far.
  run_rows   <- vector("list", length(step_specs))
  stop_after <- length(step_specs)

  for (i in seq_along(step_specs)) {
    spec <- step_specs[[i]]
    message("")
    message(strrep("-", 70))
    message("Running ", spec$step, " [", spec$type, "] : ", spec$filename)
    message(strrep("-", 70))

    result_row    <- safe_render_step(spec, code_dir)
    run_rows[[i]] <- result_row

    if (result_row$status == "error") {
      stop_after <- i
      break
    }
  }

  # Mark steps that were skipped because an upstream failure aborted
  # the chain. Their dependencies were not produced, so they cannot run.
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
    mutate(
      run_id = run_id,
      module = "03_transfer_models",
      .before = 1
    )

  pipeline_end <- Sys.time()

  full_log <- append_to_log(run_df, log_path)

  summary_df <- run_df |>
    select(script_step, step_type, status, duration_sec, n_outputs, outputs_ok, error_message)

  n_success       <- sum(run_df$status == "success")
  n_failure       <- sum(run_df$status == "error")
  n_skipped       <- sum(run_df$status == "skipped")
  total_duration  <- round(as.numeric(
    difftime(pipeline_end, pipeline_start, units = "secs")
  ), 1)

  message("")
  message(strrep("=", 70))
  message("Module 03 \u2014 Transfer Models (sequential) \u2014 Execution Summary")
  message(strrep("=", 70))
  message("Run ID:            ", run_id)
  message("Steps declared:    ", length(step_specs))
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
    "*Module 03 \u2014 Transfer Models* \u2014 finished\nRun ID: `%s`\nSuccesses: %d | Failures: %d | Skipped: %d\nTotal duration: %.1f sec",
    run_id, n_success, n_failure, n_skipped, total_duration
  ))

  invisible(run_df)
}

# --- Launch -------------------------------------------------------------------

run_pipeline()
