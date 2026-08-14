# ==============================================================================
# Local Orchestrator (mirai backend + Telegram notifications):
# Departmental Scenarios Precomputation (Step 06_02)
# ==============================================================================
#
# Parallel orchestrator over the cartesian product of seed variants,
# subsidy variants, and pathway variants. Dispatches each (seed, subsidy,
# pathway) combination to a persistent mirai daemon so that all
# combinations render concurrently. Posts start/end notifications to a
# Telegram group for unattended monitoring.
#
# Outputs per combination:
#   - 06_02_{seed_suffix}_{subsidy_suffix}_{pathway}_departmental_scenarios.parquet
#
# Prerequisites:
#   - 02_05 output: 02_05_likely_adopters_model_config.rds (continuous adoption parameters)
#   - 02_06 outputs: farmer adoption scores per (seed, subsidy) combination
#   - 06_01 outputs: market baseline and saturation scenarios per (seed, subsidy)
#   - For pathway = "pco": 04_08_baseline_qpm_*.parquet (synthetic population)
#   - For pathway = "pma": 05_06_encovi_children_priority.parquet
#
# Note: 06_02 does NOT depend on 06_03. Both scripts read the same
# upstream inputs and aggregate to different levels (06_02 departmental,
# 06_03 national). They may be executed in any order or in parallel.
#
# Telegram notifications:
#   - Reads TELEGRAM_BOT_TOKEN and TELEGRAM_CHAT_ID from .Renviron.
#   - Posts a message at pipeline start and at pipeline end (with summary).
#   - Notification failures are silent: the pipeline never aborts because
#     of a Telegram issue.
#
# Design principles:
#   - Single source of truth: seeds, subsidies, and pathways each live
#     in their own config file. The scenario grid is derived, not stored.
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
#   - Robust iteration: every combination runs inside tryCatch inside
#     its daemon. A single combination failure does not abort the batch.
#   - Error timestamps: combo_start is captured before the tryCatch
#     so that failed combinations report their real elapsed time.
#   - Atomic cumulative audit trail: log writes go to a temporary file
#     and are renamed in place, avoiding Windows memory-map collisions
#     and leaving the previous log intact on interruption.
#
# Usage:
#   source("02_code/06_scenario_precomputation/_run_all_06_02_departmental_scenarios.R")
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
        combo$seed_suffix, "_", combo$subsidy_suffix, "_", combo$pathway, "_",
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

      expected_parquet <- file.path(
        parquet_output_dir,
        paste0("06_02_", combo$seed_suffix, "_", combo$subsidy_suffix,
               "_", combo$pathway,
               "_departmental_scenarios.parquet")
      )

      parquet_exists <- file.exists(expected_parquet)

      parquet_rows <- if (parquet_exists) {
        nrow(arrow::read_parquet(expected_parquet, as_data_frame = FALSE))
      } else {
        NA_integer_
      }

      if (!parquet_exists) {
        stop("Expected parquet not generated: ", expected_parquet)
      }

      if (isTRUE(parquet_rows == 0)) {
        stop("Expected parquet is empty: ", expected_parquet)
      }

      tibble::tibble(
        seed_suffix    = combo$seed_suffix,
        seed_label     = combo$seed_label,
        subsidy_suffix = combo$subsidy_suffix,
        subsidy_label  = combo$subsidy_label,
        pathway        = combo$pathway,
        pathway_label  = combo$pathway_label,
        start_time     = combo_start,
        end_time       = combo_end,
        duration_sec   = as.numeric(
          difftime(combo_end, combo_start, units = "secs")
        ),
        status         = "success",
        error_message  = NA_character_,
        output_parquet = expected_parquet,
        output_exists  = parquet_exists,
        output_nrow    = parquet_rows
      )
    },
    error = function(e) {
      combo_end <- Sys.time()
      tibble::tibble(
        seed_suffix    = if (is.null(combo$seed_suffix))    NA_character_ else combo$seed_suffix,
        seed_label     = if (is.null(combo$seed_label))     NA_character_ else combo$seed_label,
        subsidy_suffix = if (is.null(combo$subsidy_suffix)) NA_character_ else combo$subsidy_suffix,
        subsidy_label  = if (is.null(combo$subsidy_label))  NA_character_ else combo$subsidy_label,
        pathway        = if (is.null(combo$pathway))        NA_character_ else combo$pathway,
        pathway_label  = if (is.null(combo$pathway_label))  NA_character_ else combo$pathway_label,
        start_time     = combo_start,
        end_time       = combo_end,
        duration_sec   = as.numeric(
          difftime(combo_end, combo_start, units = "secs")
        ),
        status         = "error",
        error_message  = conditionMessage(e),
        output_parquet = NA_character_,
        output_exists  = FALSE,
        output_nrow    = NA_integer_
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

  seed_variants_path    <- here("02_code", "_config", "seed_variants.yml")
  subsidy_variants_path <- here("02_code", "_config", "subsidy_variants.yml")
  pathway_variants_path <- here("02_code", "_config", "pathway_variants.yml")

  if (!file.exists(seed_variants_path)) {
    stop("Configuration file not found: ", seed_variants_path)
  }
  if (!file.exists(subsidy_variants_path)) {
    stop("Configuration file not found: ", subsidy_variants_path)
  }
  if (!file.exists(pathway_variants_path)) {
    stop("Configuration file not found: ", pathway_variants_path)
  }

  seed_variants    <- yaml::read_yaml(seed_variants_path)$variants
  subsidy_variants <- yaml::read_yaml(subsidy_variants_path)$variants
  pathway_variants <- yaml::read_yaml(pathway_variants_path)$variants

  if (length(seed_variants) == 0)    stop("No seed variants found in ",    seed_variants_path)
  if (length(subsidy_variants) == 0) stop("No subsidy variants found in ", subsidy_variants_path)
  if (length(pathway_variants) == 0) stop("No pathway variants found in ", pathway_variants_path)

  # --- Validate upstream prerequisites ----------------------------------------

  models_dir   <- here("01_data", "02_processed", "models")
  scenario_dir <- here("01_data", "02_processed", "scenarios")

  # 1. Model configuration (continuous adoption parameters from 02_05)
  model_config_path <- file.path(
    models_dir, "02_05_likely_adopters_model_config.rds"
  )
  if (!file.exists(model_config_path)) {
    stop("Model config not found (run 02_05 first): ", model_config_path)
  }

  # 2. Farmer adoption parquets: one per (seed, subsidy) combination
  seed_subsidy_combos <- tidyr::expand_grid(
    seed    = seed_variants,
    subsidy = subsidy_variants
  ) |>
    purrr::pmap(function(seed, subsidy) {
      list(seed_suffix = seed$seed_suffix, subsidy_suffix = subsidy$subsidy_suffix)
    })

  missing_inputs <- character(0)

  for (ss in seed_subsidy_combos) {
    # 02_06 farmer parquets
    farmer_path <- file.path(
      scenario_dir,
      paste0("02_06_", ss$seed_suffix, "_", ss$subsidy_suffix,
             "_farmers_adoption_scores.parquet")
    )
    if (!file.exists(farmer_path)) {
      missing_inputs <- c(missing_inputs, basename(farmer_path))
    }

    # 06_01 market baseline
    baseline_path <- file.path(
      scenario_dir,
      paste0("06_01_", ss$seed_suffix, "_", ss$subsidy_suffix,
             "_market_baseline.parquet")
    )
    if (!file.exists(baseline_path)) {
      missing_inputs <- c(missing_inputs, basename(baseline_path))
    }

    # 06_01 saturation scenarios
    saturation_path <- file.path(
      scenario_dir,
      paste0("06_01_", ss$seed_suffix, "_", ss$subsidy_suffix,
             "_saturation_scenarios.parquet")
    )
    if (!file.exists(saturation_path)) {
      missing_inputs <- c(missing_inputs, basename(saturation_path))
    }
  }

  # 3. Children population parquets (pathway-specific)
  pco_demo_path <- file.path(
    scenario_dir, "04_08_baseline_qpm_demographics.parquet"
  )
  pco_prof_path <- file.path(
    scenario_dir, "04_08_baseline_qpm_profiles.parquet"
  )
  pma_path <- file.path(
    here("01_data", "02_processed", "transfer"),
    "05_06_encovi_children_priority.parquet"
  )

  has_pco <- any(purrr::map_chr(pathway_variants, "pathway") == "pco")
  has_pma <- any(purrr::map_chr(pathway_variants, "pathway") == "pma")

  if (has_pco) {
    if (!file.exists(pco_demo_path)) {
      missing_inputs <- c(missing_inputs, basename(pco_demo_path))
    }
    if (!file.exists(pco_prof_path)) {
      missing_inputs <- c(missing_inputs, basename(pco_prof_path))
    }
  }
  if (has_pma) {
    if (!file.exists(pma_path)) {
      missing_inputs <- c(missing_inputs, basename(pma_path))
    }
  }

  if (length(missing_inputs) > 0) {
    stop(
      "Missing upstream inputs (", length(missing_inputs), " files):\n",
      paste("  -", missing_inputs, collapse = "\n")
    )
  }

  message("Pre-render validation: all upstream inputs present.")

  # --- Build combination grid ------------------------------------------------

  # Triple cartesian product: seed x subsidy x pathway.
  # Each combo carries the identity fields the .qmd declares in its
  # header YAML (seed_*, subsidy_*, pathway_*). The worker passes the
  # full combo to quarto_render via execute_params.
  combos <- tidyr::expand_grid(
    seed    = seed_variants,
    subsidy = subsidy_variants,
    pathway = pathway_variants
  ) |>
    purrr::pmap(function(seed, subsidy, pathway) {
      list(
        seed_year      = seed$seed_year,
        seed_label     = seed$seed_label,
        seed_suffix    = seed$seed_suffix,
        subsidy_label  = subsidy$subsidy_label,
        subsidy_suffix = subsidy$subsidy_suffix,
        pathway        = pathway$pathway,
        pathway_label  = pathway$pathway_label
      )
    })

  qmd_input <- here(
    "02_code", "06_scenario_precomputation",
    "06_02x_precompute_departmental_scenarios.qmd"
  )

  parquet_output_dir <- here("01_data", "02_processed", "scenarios")

  log_dir <- here("01_data", "02_processed", "orchestrator_logs")
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }

  log_path <- file.path(log_dir, "06_02_departmental_scenarios_log.parquet")

  # --- Parallelism configuration --------------------------------------------

  # CPU reservation: cores left free for the OS, the main R process,
  # and other applications running on the host. M06 scenario scripts
  # carry heavier per-render workloads (mgcv/gratia, larger datasets)
  # than M02, so we reserve more headroom.
  cores_reserved <- 3

  physical_cores <- parallel::detectCores(logical = FALSE)
  n_daemons      <- min(length(combos), max(1, physical_cores - cores_reserved))

  mirai_profile  <- "orchestrator_06_02"

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
    "*06_02 Departmental Scenarios* \u2014 pipeline started\nRun ID: `%s`\nCombinations: %d (%d seeds \u00d7 %d subsidies \u00d7 %d pathways)\nDaemons: %d",
    run_id,
    length(combos),
    length(seed_variants),
    length(subsidy_variants),
    length(pathway_variants),
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
      script_step = "06_02",
      .before     = 1
    )

  pipeline_end <- Sys.time()

  full_log <- append_to_log(run_df, log_path)

  summary_df <- run_df |>
    select(seed_suffix, subsidy_suffix, pathway, status,
           duration_sec, output_nrow, error_message)

  n_success       <- sum(run_df$status == "success")
  n_failure       <- sum(run_df$status == "error")
  total_duration  <- round(as.numeric(
    difftime(pipeline_end, pipeline_start, units = "secs")
  ), 1)

  message("")
  message(strrep("=", 70))
  message("Departmental Scenarios Pipeline (mirai) \u2014 Execution Summary")
  message(strrep("=", 70))
  message("Run ID:            ", run_id)
  message(
    "Daemons used:      ", n_daemons, " / ",
    length(combos), " combinations (",
    length(seed_variants),    " seeds \u00d7 ",
    length(subsidy_variants), " subsidies \u00d7 ",
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
    "*06_02 Departmental Scenarios* \u2014 pipeline finished\nRun ID: `%s`\nSuccesses: %d | Failures: %d\nTotal duration: %.1f sec",
    run_id, n_success, n_failure, total_duration
  ))

  invisible(run_df)
}

# --- Launch -------------------------------------------------------------------

run_pipeline()
