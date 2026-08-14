# Module 00: Data Provisioning

## Purpose

This folder contains scripts that provision and verify the data assets
used by the NewSeed biofortification framework. These scripts are
**not required** for day-to-day analysis — the framework operates on
Parquet files and pre-computed matrices that are already present in
`01_data/`.

Execute these scripts only when:

1. **Setting up the project for the first time** on a new machine
2. **Auditing data provenance** — an external auditor (e.g., GiveWell)
   wants to verify that the Parquet files used by the framework are
   faithful conversions of the official public datasets
3. **Updating raw sources** — a data provider (INE, SIINSAN) publishes
   a revised dataset
4. **Rebuilding the travel time matrix** — the OSRM routing data has
   been updated or departmental capital coordinates have changed

## Orchestrator

The recommended way to execute all provisioning scripts is via the
orchestrator:

```r
source("02_code/00_data_provisioning/_run_all_module00_provisioning.R")
```

This runs the three steps sequentially with fail-fast behaviour: if
a step fails, downstream steps are skipped. The orchestrator validates
expected outputs after each step, writes a cumulative log to
`orchestrator_logs/module00_provisioning_log.parquet`, and sends
Telegram start/end notifications.

Manual execution of individual scripts is also supported (see
Execution Order below).

## Scripts

The module contains two categories of scripts: data acquisition
(00_01, 00_02) and infrastructure pre-computation (06_00).

### Data Acquisition

These two scripts establish the complete provenance chain from
official public URLs to the Parquet files consumed by the analytical
pipeline. They are executed sequentially: download first, convert
second. Both scripts read `02_code/_config/raw_sources.yml` as their
single source of truth for which files to download and convert.

| Script | Purpose | Inputs | Outputs |
|--------|---------|--------|---------|
| `00_01_download_raw_sources.qmd` | Download original files from official URLs, verify SHA256 hashes | `raw_sources.yml` manifest | `.xlsx`/`.dta` files in `01_data/01_raw/{SOURCE}/`, hash log (`_hashes_active.csv`) |
| `00_02_convert_to_parquet.qmd` | Convert active originals to Parquet, unify multi-file reference sources (INE births, WHO growth), verify conversion fidelity, package originals into ZIP archive, remove originals | `raw_sources.yml` manifest + `.xlsx`/`.dta` files from step 1 | `.parquet` files (same directories), unified reference parquets, `_originals/active_raw_sources.zip` |

### Infrastructure Pre-Computation

| Script | Purpose | Inputs | Outputs |
|--------|---------|--------|---------|
| `06_00_compute_travel_time_matrix.qmd` | Calculate pairwise travel times between Guatemala's 22 departmental capitals using the OSRM routing engine | Departmental capital coordinates, OSRM server | `06_00_departmental_capitals.parquet`, `06_00_osrm_travel_time_matrix.parquet` |

The travel time matrix is a 22×22 asymmetric matrix of driving times
(in minutes) between all pairs of departmental capitals. Module 06
uses it in `redistribute_gravity()` to model how biofortified maize
production in surplus departments flows to deficit departments —
departments that are closer and have larger deficits receive a larger
share of the surplus. The matrix is computed once against a public
OSRM server and the result is stored as a Parquet file. It only needs
to be regenerated if the road network data in OSRM is updated or if
the departmental capital coordinates change.

The `06_00` prefix indicates its logical relationship to Module 06
(Scenario Precomputation). The script lives in `02_code/06_scenarios/`
alongside the scripts that consume its output, but it is included in
this module's orchestrator because it is a one-time provisioning step
that does not run as part of the recurrent Module 06 orchestrators.

## Execution Order

When using the orchestrator, all three steps run automatically in the
correct order. For manual execution:

```
Data acquisition (sequential, strict dependency):
  00_01_download_raw_sources.qmd  →  00_02_convert_to_parquet.qmd

Infrastructure (no dependency on 00_01/00_02):
  06_00_compute_travel_time_matrix.qmd
```

Both data acquisition scripts are idempotent: re-running them on an
already-provisioned project skips files that already exist (unless
`force_download = TRUE` or `force_convert = TRUE` is set). The travel
time matrix script is also idempotent — it overwrites the existing
matrix with a fresh computation.

## Audit Verification Chain

The two data acquisition scripts together establish a verifiable chain
from official public URLs to the Parquet files consumed by the
analytical pipeline:

1. **Source authenticity**: `00_01` downloads from authoritative URLs
   declared in `raw_sources.yml` and verifies each file's SHA256
   hash against the expected value recorded in the manifest.

2. **Conversion fidelity**: `00_02` reads the same manifest to
   identify active files, converts only those to Parquet, and
   produces a verification table confirming that row counts,
   column counts, and column names are preserved exactly.

3. **Archival**: `00_02` packages all active originals into
   `_originals/active_raw_sources.zip` before removing the
   original files from the working directories. The ZIP preserves
   the exact bytes used for conversion.

4. **Reproducibility**: An auditor who re-executes both scripts from
   scratch will obtain identical SHA256 hashes (if the upstream
   sources have not changed) and identical Parquet files.

## Data Sources

| Source | Provider | Format | Active Files | Unified Output | Used By |
|--------|----------|--------|-------------|----------------|---------|
| ENCOVI 2023 | INE Guatemala | Excel (.xlsx) | 5 files | 5 × individual parquets | Modules 01, 02 |
| SIVESNU 2018 | SESAN/INCAP via SIINSAN | Stata (.dta) | 4 files | 4 × individual parquets | Modules 03, 04, 05 |
| INE Nacimientos | INE Guatemala | Excel (.xlsx) | 5 files (2019–2023) | `ine_nacimientos_2019_2023.parquet` | Module 01 (01_03) |
| WHO Standards 0–5 | WHO | Excel (.xlsx) | 4 files (monthly) | `who_standards_lhfa_monthly.parquet` | Module 05 (05_03) |
| WHO Standards 0–5 expanded | WHO | Excel (.xlsx) | 2 files (daily) | `who_standards_lhfa_daily.parquet` | Module 05 (05_05) |
| WHO Reference 5–19 | WHO | Excel (.xlsx) | 2 files (monthly) | `who_reference_hfa_monthly.parquet` | Module 05 (05_05) |

Sources not handled by the provisioning scripts (committed to
`01_data/03_external/`):

| File | Provider | Format | Role | Used By |
|------|----------|--------|------|---------|
| `incap_finut_master.xlsx` | INCAP / FINUT (expert consultation) | Excel (.xlsx) | Editable source of truth: `food_composition`, `food_equivalences` and `source_notes` sheets | Script 00_03 |
| `incap_food_composition.parquet` | Built by Script 00_03 | Parquet | Nutrient composition, one row per food | Module 01 (01_01) |
| `incap_food_equivalences.parquet` | Built by Script 00_03 | Parquet | Unit-to-grams conversions, one row per food × unit | Module 01 (01_01) |

**Note on the nutrition tables**: The INCAP food composition tables are
not available from a public URL. The master workbook is committed to
version control and the two Parquet tables are built from it by Script
`00_03_build_nutrition_tables.qmd`, which is not part of the Module 00
orchestrator and is rendered on demand when the workbook changes. See
`01_data/03_external/README.md` for provenance and lineage.

The Gunaratna et al. (2010) meta-analysis effect size used in Module 05
is not a data file: it is declared as a constant in Script `05_05`, with
the citation in the script narrative.

## Supporting Files

| File | Location | Purpose |
|------|----------|---------|
| `_run_all_module00_provisioning.R` | `02_code/00_data_provisioning/` | Orchestrator: sequential execution of all three provisioning scripts |
| `00_functions_provisioning.R` | `02_code/00_functions/` | Utility functions for download, hashing, extraction, conversion, verification, and reference data unification |
| `raw_sources.yml` | `02_code/_config/` | Source manifest with URLs, SHA256 hashes, and metadata for all downloadable sources |

## Relationship to the Analytical Pipeline

```
Module 00 (provisioning)               Modules 01–06 (pipeline)
----------------------------           ---------------------------------
00_01 → download originals
00_02 → convert to parquet         →   01_01 reads .parquet (ENCOVI)
      → unify INE births           →   01_03 reads ine_nacimientos .parquet
      → unify WHO growth           →   02_01 reads .parquet (ENCOVI agricultural)
      → archive + remove originals     04_01 reads .parquet (SIVESNU 2018)
                                       05_03 reads who_standards_monthly .parquet
                                       05_05 reads who_standards_daily .parquet
                                       05_05 reads who_reference_monthly .parquet

06_00 → travel time matrix         →   06_01 reads travel time .parquet
                                       (used by redistribute_gravity())

00_03 → build nutrition tables     →   01_01 reads incap_food_composition .parquet
        from incap_finut_master.xlsx   01_01 reads incap_food_equivalences .parquet
        (03_external, committed;       (rendered on demand, not in the
         not in the orchestrator)       Module 00 orchestrator)
```

Module 00 is upstream of everything but entirely optional. The Parquet
files in `01_data/01_raw/` are the actual entry points for the
analytical pipeline. The travel time matrix in
`01_data/02_processed/external/` is consumed only by Module 06.
