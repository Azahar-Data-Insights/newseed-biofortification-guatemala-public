# Code Directory — Pipeline Orchestration

This directory contains the analytical pipeline of the New Seed
biofortification framework, organized into six sequential modules plus
a data-provisioning module. Execution is managed by **orchestrators**:
top-level `.R` files (prefix `_run_all_`) that render the module
scripts in dependency order, validate their outputs, write a structured
log, and post Telegram start/end notifications for unattended runs.

This document describes how the pipeline is launched. For the data
lifecycle and directory layout see `01_data/README.md`; for
data-provisioning specifics see
`02_code/00_data_provisioning/README.md`.

## Orchestration Levels

The pipeline uses three levels of orchestration. The shallowest level
that fits each step is used; nesting is applied only where a step
genuinely expands into multiple independent renders.

### Level 1 — Module orchestrators (sequential)

`_run_all_module{NN}_{name}.R`, one per module, located inside the
module folder. Executes the module's steps in strict dependency order.
Each step is either a direct `.qmd` render (single execution) or a
nested sub-orchestrator (parametric step). Module orchestrators are
always sequential at the top level; any parallelism is encapsulated
inside a sub-orchestrator.

### Level 2 — Sub-orchestrators (parallel, parametric)

`_run_all_{NN_step}_{name}.R`, located inside the module folder.
Render the same `.qmd` once per configuration combination over a
cartesian product of variant axes (seed, subsidy, pathway, nutrient),
dispatching each combination to a persistent `mirai` daemon so that
variants render concurrently. A module orchestrator invokes these with
`source(path, local = TRUE)`.

### Level 3 — Project orchestrator

`_run_all_project.R` (in `02_code/`) sources every module orchestrator
in dependency order, running the full analytical pipeline end to end.
Modules with parametric steps additionally expose their
sub-orchestrators so that a single parametric step can be re-run in
isolation without re-running the whole module.

## Orchestrators by Module

| Module | Orchestrator | Shape |
|--------|--------------|-------|
| (all) Project | `_run_all_project.R` | Sources modules 01–06 in order |
| 00 Data provisioning | `_run_all_module00_provisioning.R` | Sequential |
| 01 Nutritional intakes | `_run_all_module01_nutritional_intakes.R` | Sequential |
| 02 Economic impact | `_run_all_module02_economic_impact.R` | Sequential + 2 sub-orchestrators |
| 03 Transfer models | `_run_all_module03_transfer_models.R` | Sequential + 1 sub-orchestrator |
| 04 Nutritional impact (covariate) | `_run_all_module04_stunting_sivesnu.R` | Sequential |
| 05 Nutritional impact (meta-analysis) | `_run_all_module05_height_impact.R` | Sequential |
| 06 Scenario precomputation | `_run_all_module06_scenarios.R` | Sequential + 4 sub-orchestrators |

### Sub-orchestrators (Level 2)

| Step | Sub-orchestrator | Variant axes |
|------|------------------|--------------|
| 02_04 | `_run_all_02_04_economic_impacts.R` | seed × subsidy |
| 02_06 | `_run_all_02_06_adopter_models.R` | seed × subsidy |
| 03_0x | `_run_all_03_transfer_models.R` | nutrient |
| 06_01 | `_run_all_06_01_market_baselines.R` | seed × subsidy |
| 06_02 | `_run_all_06_02_departmental_scenarios.R` | seed × subsidy × pathway |
| 06_03 | `_run_all_06_03_national_aggregates.R` | seed × subsidy × pathway |
| 06_04 | `_run_all_06_04_consolidations.R` | pathway |

Variant axes are defined in single-source-of-truth YAML files under
`02_code/_config/` (`seed_variants.yml`, `subsidy_variants.yml`,
`pathway_variants.yml`, `nutrient_variants.yml`). Each sub-orchestrator
builds its execution grid as the cartesian product of the relevant
files at runtime.

## Running the Full Pipeline

The whole analytical pipeline (modules 01–06) runs end to end with a
single call to the project orchestrator:

```r
source(here::here("02_code", "_run_all_project.R"))
```

This sources each module orchestrator in dependency order. Each module
consumes outputs from the previous ones, so the order is mandatory and
is fixed inside the project orchestrator.

Module 00 (data provisioning) is **not** part of the default run. It
downloads and converts official raw sources and rebuilds the travel
time matrix — a one-time setup and audit task, not a routine pipeline
step. To run the pipeline from raw-source provisioning (new environment
or full reproducibility audit), set `run_provisioning <- TRUE` near the
top of `_run_all_project.R` before sourcing it. See
`00_data_provisioning/README.md` for what provisioning entails.

Individual modules can also be run on their own, in order, by sourcing
their module orchestrators directly:

```r
source(here::here(
  "02_code", "01_nutritional_intakes",
  "_run_all_module01_nutritional_intakes.R"
))
# ... and so on for modules 02 through 06, in order.
```

## Running Parametric Steps in Isolation

When only a parametric step needs to be re-run (for example, after a
change to a YAML variant file), the sub-orchestrator can be launched
directly without re-running its whole module. Respect the within-module
dependencies.

```r
# Module 02: economic impact per seed × subsidy, then adopter models
source(here::here(
  "02_code", "02_economic_impact",
  "_run_all_02_04_economic_impacts.R"
))
# 02_06 needs the outputs of 02_04
source(here::here(
  "02_code", "02_economic_impact",
  "_run_all_02_06_adopter_models.R"
))

# Module 03: transfer models by nutrient
source(here::here(
  "02_code", "03_transfer_models",
  "_run_all_03_transfer_models.R"
))

# Module 06: market baselines first (produce inputs for 06_02 and 06_03)
source(here::here(
  "02_code", "06_scenario_precomputation",
  "_run_all_06_01_market_baselines.R"
))
# 06_02 and 06_03 are independent of each other; either order is valid
source(here::here(
  "02_code", "06_scenario_precomputation",
  "_run_all_06_02_departmental_scenarios.R"
))
source(here::here(
  "02_code", "06_scenario_precomputation",
  "_run_all_06_03_national_aggregates.R"
))
# 06_04 last; consolidates 06_02 and 06_03
source(here::here(
  "02_code", "06_scenario_precomputation",
  "_run_all_06_04_consolidations.R"
))
```

## Design Principles

All orchestrators follow the ADI orchestration standard:

- **Sequential modules, parallel variants.** Module orchestrators run
  steps in strict dependency order. Parallelism via `mirai` daemons is
  confined to the parametric sub-orchestrators, where renders are
  genuinely independent.
- **Fail-fast.** Because each step depends on the previous one, a
  failure aborts the remaining steps; the log records the failed step
  and those skipped as a consequence.
- **Validated outputs.** After each step the orchestrator checks that
  the expected Parquet (or RDS) outputs exist before proceeding.
- **Structured logging.** Each orchestrator writes a cumulative log to
  `01_data/02_processed/orchestrator_logs/`, written atomically to
  survive interruptions.
- **Telegram notifications.** Start and end messages are posted to a
  Telegram group using credentials read from `.Renviron`
  (`TELEGRAM_BOT_TOKEN`, `TELEGRAM_CHAT_ID`). Notification failures are
  silent and never abort the pipeline. Nested sub-orchestrators emit
  their own start/end messages.
- **Parquet-only framework outputs.** Orchestrators produce data files.
  The HTML render is a `quarto_render()` by-product and is cleaned up on
  exit together with `.knit.md` and `_files/` artefacts. The
  methodological website has its own separate rendering pipeline.

Shared helper functions used by all orchestrators
(`notify_telegram()`, `append_to_log()`, step workers, cartesian-grid
builder) live in `02_code/00_functions/`.

## Configuration Files

| File | Defines |
|------|---------|
| `_config/seed_variants.yml` | Seed varieties (s22, s26, s28) and yield-change factors |
| `_config/subsidy_variants.yml` | Subsidy regimes and seed prices |
| `_config/pathway_variants.yml` | Stunting-impact pathways (covariate, meta-analysis) |
| `_config/nutrient_variants.yml` | Nutrients modelled in the transfer step |
| `_config/raw_sources.yml` | Official raw-source registry for Module 00 |
