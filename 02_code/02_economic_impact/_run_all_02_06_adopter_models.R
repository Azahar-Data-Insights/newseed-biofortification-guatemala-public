# ==============================================================================
# Local Orchestrator (mirai backend + Telegram notifications):
# Likely Adopter Modeling (Step 02_06)
# ==============================================================================
#
# Parallel orchestrator over the cartesian product of seed variants and
# subsidy variants. Dispatches each (seed, subsidy) combination to a
# persistent mirai daemon so that all combinations render concurrently.
# Posts start/end notifications to a Telegram group for unattended
# monitoring.
#
# Outputs per combination:
#   - 02_06_{seed_suffix}_{subsidy_suffix}_farmers_adoption_scores.parquet
#
# Prerequisites:
#   - 02_04 outputs: economic impact per farmer per (seed, subsidy) combination
#   - 02_03 outputs: farmer segmentation dataset
#   - 02_05 output:  likely adopters model configuration (.rds)
#
# Telegram notifications:
#   - Reads TELEGRAM_BOT_TOKEN and TELEGRAM_CHAT_ID from .Renviron.
#   - Posts a message at pipeline start and at pipeline end (with summary).
#   - Notification failures are silent: the pipeline never aborts because
#     of a Telegram issue.
#
# Design principles:
#   - Single source of truth: seeds live in seed_variants.yml, subsidies
#     in subsidy_variants.yml. The orchestrator forms the cartesian
#     product at runtime; configuration files remain orthogonal.
#   - Flattened params: 02_06x consumes only the identity fields of a
#     (seed, subsidy) combination; yield parameters and seed prices are
#     already baked into the economic-impact parquet produced by 02_04.
#     The worker selects the fields that the .qmd declares in its
#     header YAML.
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
#   - Honest error timestamps: combo_start is captured before the
#     tryCatch so that failed combinations report their real elapsed
#     time, not a microsecond-level artefact of the error handler.
#   - Atomic cumulative audit trail: log writes go to a temporary file
#     and are renamed in place, avoiding Windows memory-map collisions
#     and leaving the previous log intact on interruption.
#
# Usage:
#   source("02_code/02_economic_impact/_run_all_02_06_adopter_models.R")
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

  # Captured before the tryCatch so failing combinations report their
  # real elapsed time.
  combo_start <- Sys.time()

  tryCatch(
    {
      # --- Build a per-daemon unique copy of the .qmd -------------------

      qmd_dir  <- dirname(qmd_input)
      qmd_stem <- tools::file_path_sans_ext(basename(qmd_input))
      qmd_ext  <- tools::file_ext(basename(qmd_input))

      unique_tag <- paste0(
        combo$seed_suffix, "_", combo$subsidy_suffix, "_",
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
      # 02_06x only consumes identity fields. Yield factors and seed
      # prices are already baked into the parquet produced by 02_04.

      execute_params <- list(
        # Seed
        seed_year      = combo$seed_year,
        seed_label     = combo$seed_label,
        seed_suffix    = combo$seed_suffix,
        # Subsidy
        subsidy_label  = combo$subsidy_label,
        subsidy_suffix = combo$subsidy_suffix
      )

      # --- Render -------------------------------------------------------

      quarto::quarto_render(
        input          = qmd_local,
        execute_params = execute_params,
        quiet          = FALSE
      )

      combo_end <- Sys.time()

      # --- Post-execution validation ------------------------------------

      expected_parquet <- file.path(
        parquet_output_dir,
        paste0(
          "02_06_", combo$seed_suffix, "_", combo$subsidy_suffix,
          "_farmers_adoption_scores.parquet"
        )
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
        seed_suffix     = combo$seed_suffix,
        seed_label      = combo$seed_label,
        subsidy_suffix  = combo$subsidy_suffix,
        subsidy_label   = combo$subsidy_label,
        start_time      = combo_start,
        end_time        = combo_end,
        duration_sec    = as.numeric(
          difftime(combo_end, combo_start, units = "secs")
        ),
        status          = "success",
        error_message   = NA_character_,
        output_parquet  = expected_parquet,
        output_exists   = parquet_exists,
        output_nrow     = parquet_rows
      )
    },
    error = function(e) {
      combo_end <- Sys.time()
      tibble::tibble(
        seed_suffix     = if (is.null(combo$seed_suffix))    NA_character_ else combo$seed_suffix,
        seed_label      = if (is.null(combo$seed_label))     NA_character_ else combo$seed_label,
        subsidy_suffix  = if (is.null(combo$subsidy_suffix)) NA_character_ else combo$subsidy_suffix,
        subsidy_label   = if (is.null(combo$subsidy_label))  NA_character_ else combo$subsidy_label,
        start_time      = combo_start,
        end_time        = combo_end,
        duration_sec    = as.numeric(
          difftime(combo_end, combo_start, units = "secs")
        ),
        status          = "error",
        error_message   = conditionMessage(e),
        output_parquet  = NA_character_,
        output_exists   = FALSE,
        output_nrow     = NA_integer_
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

# --- Cartesian product builder ------------------------------------------------

build_combos <- function(seed_variants, subsidy_variants) {

  # Each combination is a flat list combining one seed variant with one
  # subsidy variant. 02_06x only needs the identity fields of each
  # variant; the cost parameters (newseed_subsidy_per_mz, seed_cost_*,
  # bio_cost_no_subsidy_per_mz) are not propagated because they are
  # already encoded in the 02_04 parquet read by 02_06x.

  combo_grid <- expand.grid(
    seed_key    = names(seed_variants),
    subsidy_key = names(subsidy_variants),
    KEEP.OUT.ATTRS  = FALSE,
    stringsAsFactors = FALSE
  )

  combos_list <- purrr::map2(
    combo_grid$seed_key,
    combo_grid$subsidy_key,
    function(s_key, sub_key) {
      seed_v <- seed_variants[[s_key]]
      sub_v  <- subsidy_variants[[sub_key]]
      c(seed_v, sub_v)
    }
  )

  names(combos_list) <- paste0(combo_grid$seed_key, "__", combo_grid$subsidy_key)
  combos_list
}

# --- Pipeline body ------------------------------------------------------------

run_pipeline <- function() {

  seed_variants_path    <- here("02_code", "_config", "seed_variants.yml")
  subsidy_variants_path <- here("02_code", "_config", "subsidy_variants.yml")

  if (!file.exists(seed_variants_path)) {
    stop("Configuration file not found: ", seed_variants_path)
  }
  if (!file.exists(subsidy_variants_path)) {
    stop("Configuration file not found: ", subsidy_variants_path)
  }

  seed_variants    <- yaml::read_yaml(seed_variants_path)$variants
  subsidy_variants <- yaml::read_yaml(subsidy_variants_path)$variants

  if (length(seed_variants) == 0) {
    stop("No seed variants found in ", seed_variants_path)
  }
  if (length(subsidy_variants) == 0) {
    stop("No subsidy variants found in ", subsidy_variants_path)
  }

  # --- Validate that the calibrated model artefact exists -------------------
  #
  # 02_06x reads the model configuration produced by 02_05 inside its
  # setup chunk. Checking here surfaces the missing prerequisite once,
  # in the parent process.

  model_config_path <- here(
    "01_data", "02_processed", "models",
    "02_05_likely_adopters_model_config.rds"
  )
  if (!file.exists(model_config_path)) {
    stop(
      "Likely adopters model configuration not found: ", model_config_path,
      "\nRun 02_05_determine_likely_adopters_model.qmd before this orchestrator."
    )
  }

  combos <- build_combos(seed_variants, subsidy_variants)

  # --- Paths ----------------------------------------------------------------

  qmd_input <- here(
    "02_code", "02_economic_impact",
    "02_06x_apply_likely_adopters_model.qmd"
  )

  parquet_output_dir <- here("01_data", "02_processed", "scenarios")

  log_dir <- here("01_data", "02_processed", "orchestrator_logs")
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }

  log_path <- file.path(log_dir, "02_06_adopter_models_log.parquet")

  # --- Parallelism configuration --------------------------------------------

  # CPU reservation: cores left free for the OS, the main R process,
  # and other applications running on the host. For lightweight scripts
  # (M02), 2-3 is sufficient. For heavy scripts that themselves
  # parallelize internally (M04, M05 with missForest/ranger), 4 is safer.
  cores_reserved <- 3

  physical_cores <- parallel::detectCores(logical = FALSE)
  n_daemons      <- min(length(combos), max(1, physical_cores - cores_reserved))

  mirai_profile  <- "orchestrator_02_06"

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
    "*02_06 Adopter Models* \u2014 pipeline started\nRun ID: `%s`\nCombinations: %d (%d seeds \u00d7 %d subsidies)\nDaemons: %d",
    run_id,
    length(combos),
    length(seed_variants),
    length(subsidy_variants),
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
      script_step = "02_06",
      .before     = 1
    )

  pipeline_end <- Sys.time()

  full_log <- append_to_log(run_df, log_path)

  summary_df <- run_df |>
    select(
      seed_suffix, subsidy_suffix, status, duration_sec,
      output_nrow, error_message
    )

  n_success       <- sum(run_df$status == "success")
  n_failure       <- sum(run_df$status == "error")
  total_duration  <- round(as.numeric(
    difftime(pipeline_end, pipeline_start, units = "secs")
  ), 1)

  message("")
  message(strrep("=", 70))
  message("Adopter Models Pipeline (mirai) \u2014 Execution Summary")
  message(strrep("=", 70))
  message("Run ID:            ", run_id)
  message(
    "Daemons used:      ", n_daemons, " / ",
    length(combos), " combinations (",
    length(seed_variants), " seeds \u00d7 ",
    length(subsidy_variants), " subsidies)"
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
    "*02_06 Adopter Models* \u2014 pipeline finished\nRun ID: `%s`\nSuccesses: %d | Failures: %d\nTotal duration: %.1f sec",
    run_id, n_success, n_failure, total_duration
  ))

  invisible(run_df)
}

# --- Launch -------------------------------------------------------------------

run_pipeline()
