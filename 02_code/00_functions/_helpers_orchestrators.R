# ==============================================================================
# Shared helpers for orchestrators
# ==============================================================================
#
# Source this file at the top of every _run_all_*.R orchestrator. It
# provides functions that run in the host R session (not inside mirai
# daemons): Telegram notifications, atomic log append, the module
# step worker, and the cartesian product builder.
#
# Functions executed inside mirai daemons (specifically safe_render_combo
# in parametric sub-orchestrators) MUST remain inline in the orchestrator
# itself. Daemons run in isolated environments and helpers sourced into
# the host process are not visible there. Either inline the worker or
# load it explicitly via mirai::everywhere({ source(...) }).
#
# Required packages in the parent script:
#   - arrow    (write_parquet, read_parquet)
#   - here     (path resolution; not used here directly but expected
#               by callers when constructing log_path)
#   - httr2    (Telegram API)
#   - quarto   (quarto_render in safe_render_step)
#   - tidyverse / tibble / dplyr (tibble construction, bind_rows)
#
# These must be loaded by the calling orchestrator via pacman::p_load()
# before sourcing this file. This file does not load packages itself
# to keep its scope narrow.
#
# Usage in the orchestrator:
#   pacman::p_load(arrow, here, httr2, quarto, tidyverse, ...)
#   source(here("02_code", "00_functions", "_helpers_orchestrators.R"))
#
# ==============================================================================


# --- Telegram notifications ---------------------------------------------------

# notify_telegram(text)
# Posts a Markdown-formatted message to the Telegram chat configured
# in .Renviron. Returns invisible(TRUE) on success, invisible(FALSE)
# on missing credentials or any error. Never throws.
#
# Reads:
#   - TELEGRAM_BOT_TOKEN
#   - TELEGRAM_CHAT_ID
#
# If either env var is missing or empty, returns invisible(FALSE)
# without raising. The pipeline never aborts because Telegram is
# unreachable, the token is rotated, or the network is flaky.
#
# Timeout: 10 seconds.
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


# --- Atomic cumulative log append --------------------------------------------

# append_to_log(new_rows, log_path)
# Appends new_rows to the parquet log at log_path. Writes via a .tmp
# file and renames in place: Windows-safe, leaves the previous log
# intact on interruption.
#
# Arguments:
#   new_rows  - tibble of rows to append
#   log_path  - character, path to the parquet log file
#
# Returns: the combined tibble (previous + new_rows).
append_to_log <- function(new_rows, log_path) {
  if (file.exists(log_path)) {
    previous <- arrow::read_parquet(log_path) |> tibble::as_tibble()
    gc(verbose = FALSE)
    combined <- dplyr::bind_rows(previous, new_rows)
  } else {
    combined <- new_rows
  }

  tmp_path <- paste0(log_path, ".tmp")
  arrow::write_parquet(combined, tmp_path)

  if (file.exists(log_path)) file.remove(log_path)
  file.rename(tmp_path, log_path)

  combined
}


# --- Module step worker (sequential orchestrator) -----------------------------

# safe_render_step(step_spec, code_dir)
# Executes one step of a module orchestrator. Handles two step types:
#   - "qmd"             : direct Quarto render
#   - "suborchestrator" : nested _run_all_*.R sourced into a clean env
#
# step_spec must be a list with elements:
#   - step             : character, step code (e.g., "02_01")
#   - filename         : character, .qmd or .R file relative to code_dir
#   - type             : "qmd" or "suborchestrator"
#   - expected_outputs : character vector of output file paths that
#                        must exist on disk after execution
#
# Returns a one-row tibble with execution metadata (status, timing,
# error message if any). Never throws: errors are caught and reflected
# in the status column.
#
# Side effects:
#   - For "qmd" steps: schedules cleanup of .html, .knit.md, _files/
#     artefacts via on.exit(); sets and restores NO_COLOR env var.
#   - For "suborchestrator" steps: sources the file into a fresh
#     environment whose parent is globalenv(), so the sub-orchestrator's
#     internal helpers do not contaminate this function's namespace.
#
# This function runs in the host R session only. It MUST NOT be
# dispatched to mirai daemons.
safe_render_step <- function(step_spec, code_dir) {

  # Captured before tryCatch so failed steps report real elapsed time.
  step_start <- Sys.time()

  tryCatch(
    {
      step_input <- file.path(code_dir, step_spec$filename)

      if (!file.exists(step_input)) {
        stop("Step file not found: ", step_input)
      }

      # --- Dispatch on step type ----------------------------------------

      if (identical(step_spec$type, "qmd")) {

        # Schedule cleanup of rendering artefacts.
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

        # Suppress ANSI color codes in Quarto CLI output. Positron and
        # RStudio render stderr in red; without this, successful runs
        # look like errors.
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

        quarto::quarto_render(input = step_input, quiet = FALSE)

      } else if (identical(step_spec$type, "suborchestrator")) {

        # Invoke nested sub-orchestrator in an isolated environment.
        # Its internal helpers live in suborchestrator_env and do not
        # contaminate the module orchestrator's namespace.
        suborchestrator_env <- new.env(parent = globalenv())
        sys.source(step_input, envir = suborchestrator_env)

      } else {
        stop("Unknown step type: ", step_spec$type,
             " (expected 'qmd' or 'suborchestrator')")
      }

      step_end <- Sys.time()

      # Post-execution validation: every declared output must exist.
      expected_outputs <- step_spec$expected_outputs
      outputs_exist    <- vapply(expected_outputs, file.exists, logical(1))

      if (!all(outputs_exist)) {
        missing <- expected_outputs[!outputs_exist]
        stop("Expected outputs not generated: ",
             paste(missing, collapse = ", "))
      }

      tibble::tibble(
        script_step   = step_spec$step,
        step_type     = step_spec$type,
        step_filename = step_spec$filename,
        start_time    = step_start,
        end_time      = step_end,
        duration_sec  = as.numeric(difftime(step_end, step_start, units = "secs")),
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
        duration_sec  = as.numeric(difftime(step_end, step_start, units = "secs")),
        status        = "error",
        error_message = conditionMessage(e),
        n_outputs     = length(step_spec$expected_outputs),
        outputs_ok    = FALSE
      )
    }
  )
}


# --- Cartesian product builder (parametric sub-orchestrators) ----------------

# build_combos(...)
# Builds a named list of combinations from N variant axes. Accepts
# any number of named axis arguments; each must be a named list of
# variant entries (typically loaded from a YAML config).
#
# Example:
#   axis_a <- yaml::read_yaml("...")$variants
#   axis_b <- yaml::read_yaml("...")$variants
#   combos <- build_combos(axis_a = axis_a, axis_b = axis_b)
#
# Each combo is a flat list merging the entries from each axis. List
# names are concatenations of the per-axis variant keys joined with
# double underscores (e.g., "variant_x__variant_y").
#
# Top-level constants shared across variants of an axis (typically
# read from the same YAML's top level) should be attached to combos
# by the caller, not by this helper.
build_combos <- function(...) {

  axes <- list(...)

  if (length(axes) < 1) {
    stop("build_combos() requires at least one axis.")
  }

  for (axis_name in names(axes)) {
    if (length(axes[[axis_name]]) == 0) {
      stop("Axis '", axis_name, "' is empty.")
    }
  }

  # Build the cartesian product over axis keys.
  axis_keys <- lapply(axes, names)
  combo_grid <- do.call(
    expand.grid,
    c(axis_keys, list(KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE))
  )

  # For each row of the grid, merge the corresponding variant entries
  # across all axes into a single flat list.
  combos_list <- lapply(seq_len(nrow(combo_grid)), function(i) {
    parts <- lapply(names(axes), function(axis_name) {
      key <- combo_grid[[axis_name]][i]
      axes[[axis_name]][[key]]
    })
    do.call(c, parts)
  })

  # Name combos by concatenating the per-axis keys with "__".
  combo_names <- apply(combo_grid, 1, paste, collapse = "__")
  names(combos_list) <- combo_names

  combos_list
}
