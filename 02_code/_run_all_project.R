# ==============================================================================
# Project Orchestrator: New Seed Biofortification Framework
# ==============================================================================
#
# Runs the full analytical pipeline end to end by sourcing each module
# orchestrator in dependency order. Each module orchestrator is
# self-contained: it clears the workspace on entry, manages its own
# Telegram notifications, validates its own outputs, and writes its own
# cumulative log. This project orchestrator only sequences them.
#
# Scope: modules 01 through 06 (the analytical pipeline).
#   - 01  Nutritional intakes
#   - 02  Economic impact            (+ sub-orchestrators 02_04, 02_06)
#   - 03  Transfer models            (+ sub-orchestrator 03_0x)
#   - 04  Nutritional impact (covariate pathway)
#   - 05  Nutritional impact (meta-analysis pathway)
#   - 06  Scenario precomputation    (+ sub-orchestrators 06_01..06_04)
#
# Module 00 (data provisioning) is NOT part of the default run. It
# downloads and converts official raw sources and rebuilds the travel
# time matrix — a one-time setup and audit task, not a routine pipeline
# step.
# To run the pipeline from raw-source provisioning (new environment or
# full reproducibility audit), set run_provisioning to TRUE below.
#
# Design notes:
#   - Each module orchestrator begins with rm(list = ls()), so no state
#     is shared between modules and this script must not rely on
#     variables surviving across source() calls. The provisioning flag
#     is therefore read and acted upon before the module chain starts.
#   - Fail-fast is per module: if a module orchestrator aborts, this
#     script stops at that module (the error propagates). Modules
#     already executed keep their outputs and logs.
#   - Telegram start/end notifications are emitted per module by each
#     module orchestrator; this script adds no notifications of its own.
#
# Usage:
#   source(here::here("02_code", "_run_all_project.R"))
# ==============================================================================

# --- Optional: raw-source provisioning (Module 00) ----------------------------
# Set to TRUE only for a new environment or a full reproducibility audit.
run_provisioning <- FALSE

if (run_provisioning) {
  source(here::here(
    "02_code", "00_data_provisioning",
    "_run_all_module00_provisioning.R"
  ))
}

# --- Analytical pipeline (Modules 01 -> 06) -----------------------------------

# Module 01 — Nutritional intakes
source(here::here(
  "02_code", "01_nutritional_intakes",
  "_run_all_module01_nutritional_intakes.R"
))

# Module 02 — Economic impact
source(here::here(
  "02_code", "02_economic_impact",
  "_run_all_module02_economic_impact.R"
))

# Module 03 — Transfer models
source(here::here(
  "02_code", "03_transfer_models",
  "_run_all_module03_transfer_models.R"
))

# Module 04 — Nutritional impact (covariate pathway)
source(here::here(
  "02_code", "04_nutritional_impact_covariate",
  "_run_all_module04_stunting_sivesnu.R"
))

# Module 05 — Nutritional impact (meta-analysis pathway)
source(here::here(
  "02_code", "05_nutritional_impact_meta_analysis",
  "_run_all_module05_height_impact.R"
))

# Module 06 — Scenario precomputation
source(here::here(
  "02_code", "06_scenario_precomputation",
  "_run_all_module06_scenarios.R"
))
