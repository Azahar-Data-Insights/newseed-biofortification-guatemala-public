# ==============================================================================
# Local Orchestrator (sequential + Telegram notifications):
# Module 06 - Scenario Precomputation
# ==============================================================================
#
# Sequential orchestrator over the four steps of Module 06. Each step is
# itself a mirai-based sub-orchestrator that manages its own daemon pool,
# parallelism, and log. This module orchestrator invokes each one in
# dependency order and enforces fail-fast semantics: a failure in any
# step aborts the remaining steps.
#
# Scope: four pipeline steps executed in dependency order.
#   - 06_01  Market baselines                  (sub-orchestrator, seed x subsidy)
#   - 06_02  Departmental scenarios            (sub-orchestrator, seed x subsidy x pathway)
#   - 06_03  National aggregates               (sub-orchestrator, seed x subsidy x pathway)
#   - 06_04  Consolidation and overview        (sub-orchestrator, pathway)
#
# Dependency chain:
#   06_01 → 06_02 (needs market_baseline + saturation from 06_01)
#   06_01 → 06_03 (needs market_baseline + saturation from 06_01)
#   06_02 → 06_04 (needs departmental parquets from 06_02)
#   06_03 → 06_04 (needs national parquets from 06_03)
#
#   06_02 and 06_03 are independent of each other but are executed
#   sequentially to avoid saturating the host with concurrent mirai
#   daemon pools. Each sub-orchestrator already uses all available
#   cores internally.
#
# Outputs (under 01_data/02_processed/scenarios/):
#   - 06_01: 9 market_baseline + 9 saturation_scenarios parquets
#   - 06_02: 18 departmental_scenarios parquets (9 combos x 2 pathways)
#   - 06_03: 18 national_scenarios parquets (9 combos x 2 pathways)
#   - 06_04: 2 consolidated_dept + 2 consolidated_nat + 2 overview parquets + 2 CSV
#
# Prerequisites:
#   - 02_05 output: 02_05_likely_adopters_model_config.rds
#   - 02_06 outputs: farmer adoption scores per (seed, subsidy) combination
#   - For PCO pathway: 04_08_baseline_qpm_*.parquet (synthetic population)
#   - For PMA pathway: 05_06_encovi_children_priority.parquet
#   - Config files: seed_variants.yml, subsidy_variants.yml, pathway_variants.yml
#
# Telegram notifications:
#   - Reads TELEGRAM_BOT_TOKEN and TELEGRAM_CHAT_ID from .Renviron.
#   - Posts a message at module start and at module end (with summary).
#   - Nested sub-orchestrators emit their own start/end notifications.
#     Duplication is expected and tolerated: step-level visibility is
#     preferred over muting.
#   - Notification failures are silent: the pipeline never aborts because
#     of a Telegram issue.
#
# Design principles:
#   - Sequential execution: steps share a strict dependency chain.
#     No parallelization at module level; parallelism is encapsulated
#     inside each sub-orchestrator's mirai daemon pool.
#   - Sub-orchestrator invocation via isolated source: each
#     sub-orchestrator is invoked with sys.source(path, envir) into
#     a fresh environment, so its internal helpers (notify_telegram,
#     safe_render_combo, append_to_log) do not contaminate the module
#     orchestrator's namespace.
#   - Function-scoped execution: the pipeline body runs inside
#     run_pipeline(). This gives on.exit() a proper function frame so
#     that teardown (Telegram end-notification, log flush) happens
#     exactly once, even if a step aborts.
#   - Self-contained worker function: safe_render_step() contains all
#     sub-orchestrator invocation and validation logic inline, with no
#     free references to other user-defined functions.
#   - Fail-fast semantics: because each step depends on the previous
#     one (or on a prior step), a failure aborts the remaining steps.
#     The log records the failed step and the ones skipped.
#   - Step-level timing granularity: the module log captures start_time,
#     end_time, and duration_sec per step as a single block. Sub-step
#     timing (per combination) lives in each sub-orchestrator's own
#     log, queryable independently.
#   - Data-output-only validation: expected_outputs cover final data
#     deliverables only (parquets under scenarios/). Intermediate .rds
#     files under web_resources/ are not validated at module level.
#   - Error timestamps: step_start is captured before the tryCatch so
#     that failed steps report their real elapsed time.
#   - Atomic cumulative audit trail: log writes go to a temporary file
#     and are renamed in place, avoiding Windows memory-map collisions
#     and leaving the previous log intact on interruption.
#
# Usage:
#   source("02_code/06_scenario_precomputation/_run_all_module06_scenarios.R")
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

  # Captured before the tryCatch so failing steps still report their
  # real elapsed time.
  step_start <- Sys.time()

  tryCatch(
    {
      step_input <- file.path(code_dir, step_spec$filename)

      if (!file.exists(step_input)) {
        stop("Step file not found: ", step_input)
      }

      # --- Invoke nested sub-orchestrator --------------------------------
      #
      # sys.source(envir = new.env(parent = globalenv())) isolates the
      # sub-orchestrator's internal helpers from this function's namespace.
      # Each sub-orchestrator manages its own daemons, rendering artefacts
      # cleanup, and log. Its final notify_telegram() call is emitted as
      # part of the natural step duplication described in the header.

      suborchestrator_env <- new.env(parent = globalenv())
      sys.source(step_input, envir = suborchestrator_env)

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
          paste(basename(missing), collapse = ", ")
        )
      }

      tibble::tibble(
        script_step   = step_spec$step,
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

  code_dir <- here("02_code", "06_scenario_precomputation")

  scenario_dir <- here("01_data", "02_processed", "scenarios")

  log_dir <- here("01_data", "02_processed", "orchestrator_logs")
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }

  log_path <- file.path(log_dir, "module06_scenarios_log.parquet")

  # --- Read config files to enumerate expected outputs ----------------------

  seed_variants_path    <- here("02_code", "_config", "seed_variants.yml")
  subsidy_variants_path <- here("02_code", "_config", "subsidy_variants.yml")
  pathway_variants_path <- here("02_code", "_config", "pathway_variants.yml")

  for (cfg in c(seed_variants_path, subsidy_variants_path, pathway_variants_path)) {
    if (!file.exists(cfg)) stop("Configuration file not found: ", cfg)
  }

  seed_suffixes    <- yaml::read_yaml(seed_variants_path)$variants    |> purrr::map_chr("seed_suffix")
  subsidy_suffixes <- yaml::read_yaml(subsidy_variants_path)$variants |> purrr::map_chr("subsidy_suffix")
  pathway_ids      <- yaml::read_yaml(pathway_variants_path)$variants |> purrr::map_chr("pathway")

  # --- Build expected outputs per step --------------------------------------
  #
  # Each step declares the parquets it must produce. Post-execution
  # validation checks existence on disk. The module orchestrator
  # validates only the final data deliverables (parquets under
  # scenarios/); intermediate artefacts (web_resources .rds, HTML
  # renders) are the sub-orchestrator's responsibility.

  # 06_01: 2 parquets per (seed, subsidy) = 9 combos = 18 parquets
  outputs_06_01 <- tidyr::expand_grid(
    seed = seed_suffixes,
    sub  = subsidy_suffixes
  ) |>
    purrr::pmap(function(seed, sub) {
      c(
        file.path(scenario_dir, paste0("06_01_", seed, "_", sub, "_market_baseline.parquet")),
        file.path(scenario_dir, paste0("06_01_", seed, "_", sub, "_saturation_scenarios.parquet"))
      )
    }) |>
    unlist()

  # 06_02: 1 parquet per (seed, subsidy, pathway) = 18 combos
  outputs_06_02 <- tidyr::expand_grid(
    seed    = seed_suffixes,
    sub     = subsidy_suffixes,
    pathway = pathway_ids
  ) |>
    purrr::pmap_chr(function(seed, sub, pathway) {
      file.path(scenario_dir, paste0("06_02_", seed, "_", sub, "_", pathway, "_departmental_scenarios.parquet"))
    })

  # 06_03: 1 parquet per (seed, subsidy, pathway) = 18 combos
  outputs_06_03 <- tidyr::expand_grid(
    seed    = seed_suffixes,
    sub     = subsidy_suffixes,
    pathway = pathway_ids
  ) |>
    purrr::pmap_chr(function(seed, sub, pathway) {
      file.path(scenario_dir, paste0("06_03_", seed, "_", sub, "_", pathway, "_national_scenarios.parquet"))
    })

  # 06_04: 3 parquets per pathway = 2 pathways = 6 parquets
  outputs_06_04 <- purrr::map(pathway_ids, function(pw) {
    c(
      file.path(scenario_dir, paste0("06_04_", pw, "_departmental_scenarios_consolidated.parquet")),
      file.path(scenario_dir, paste0("06_04_", pw, "_national_scenarios_consolidated.parquet")),
      file.path(scenario_dir, paste0("06_04_", pw, "_overview_table.parquet"))
    )
  }) |>
    unlist()

  # --- Step specifications --------------------------------------------------

  step_specs <- list(
    list(
      step             = "06_01",
      filename         = "_run_all_06_01_market_baselines.R",
      expected_outputs = outputs_06_01
    ),
    list(
      step             = "06_02",
      filename         = "_run_all_06_02_departmental_scenarios.R",
      expected_outputs = outputs_06_02
    ),
    list(
      step             = "06_03",
      filename         = "_run_all_06_03_national_aggregates.R",
      expected_outputs = outputs_06_03
    ),
    list(
      step             = "06_04",
      filename         = "_run_all_06_04_consolidations.R",
      expected_outputs = outputs_06_04
    )
  )

  # --- Execute --------------------------------------------------------------

  run_id         <- format(Sys.time(), "%Y%m%d_%H%M%S")
  pipeline_start <- Sys.time()

  n_total_outputs <- sum(purrr::map_int(step_specs, ~ length(.x$expected_outputs)))

  notify_telegram(sprintf(
    paste0(
      "*Module 06 \u2014 Scenario Precomputation* \u2014 started\n",
      "Run ID: `%s`\n",
      "Steps: %d (sequential, each with internal mirai parallelism)\n",
      "Total expected outputs: %d parquets"
    ),
    run_id,
    length(step_specs),
    n_total_outputs
  ))

  # Fail-fast sequential execution
  run_rows   <- vector("list", length(step_specs))
  stop_after <- length(step_specs)

  for (i in seq_along(step_specs)) {
    spec <- step_specs[[i]]
    message("")
    message(strrep("-", 70))
    message("Running ", spec$step, " [sub-orchestrator] : ", spec$filename)
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
      module = "06_scenarios",
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
  message("Module 06 \u2014 Scenario Precomputation (sequential) \u2014 Execution Summary")
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
    paste0(
      "*Module 06 \u2014 Scenario Precomputation* \u2014 finished\n",
      "Run ID: `%s`\n",
      "Successes: %d | Failures: %d | Skipped: %d\n",
      "Total duration: %.1f sec"
    ),
    run_id, n_success, n_failure, n_skipped, total_duration
  ))

  invisible(run_df)
}

# --- Launch -------------------------------------------------------------------

run_pipeline()
