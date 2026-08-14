# Raw Data Directory

This directory ships nearly empty. In the public repository it contains
two files and nothing else: this README and `_hashes_active.csv`. Every
survey and reference dataset described below is downloaded from its
official provider and converted by Module 00; none of it is
redistributed here.

What follows is therefore a specification, not an inventory: it
documents the tree that Module 00 creates, which files each source
contributes, and which modules consume them. No file in this directory
is ever hand-edited.

The hash log is shipped deliberately. It is what makes the
provisioning step auditable: it records the SHA256 of every original
file as downloaded on a stated date, so anyone reproducing the pipeline
can confirm they obtained byte-identical inputs.

## Provisioning

Run the Module 00 orchestrator to populate this directory:

```r
source(here::here("02_code", "00_data_provisioning",
                  "_run_all_module00_provisioning.R"))
```

Or execute the scripts individually in order:

1. `00_01_download_raw_sources.qmd` — downloads originals from public URLs
2. `00_02_convert_to_parquet.qmd` — converts to Parquet, verifies fidelity

Both scripts read `02_code/_config/raw_sources.yml`, the source
manifest that holds the download URL, the expected SHA256 and the
conversion status of every file. It is distinct from
`_hashes_active.csv`, the hash log written by `00_01` and described
below.

## Directory Structure After Provisioning

```
01_raw/
├── ENCOVI_2023/          # 5 parquet files (food, households, persons, agriculture)
├── SIVESNU_2018/         # 4 parquet files (household, members, women, children)
├── INE_NACIMIENTOS/      # 1 parquet file (birth registry 2019–2023, unified by 00_02)
├── WHO_GROWTH/           # 3 parquet files (growth standards, unified by 00_02)
├── _originals/           # active_raw_sources.zip (pre-conversion originals)
├── _hashes_active.csv    # SHA256 hash log from 00_01 — shipped with the repository
└── README.md             # shipped with the repository
```

Only the last two entries exist in a fresh clone. The four source
subdirectories and `_originals/` are created by Module 00 and are
absent until it has run.

## Data Sources

The tables below list the Parquet files as they exist after conversion
— these are the names the analytical pipeline reads. The original
download formats (`.xlsx`, `.dta`) are recorded in
`_hashes_active.csv` and archived in `_originals/`.

### ENCOVI 2023 (Encuesta Nacional de Condiciones de Vida)

**Provider:** Instituto Nacional de Estadística (INE), Guatemala
**Landing page:** https://www.ine.gob.gt/pobreza-menu/
**Original format:** 5 xlsx files
**Redistribution:** not distributed here; download from the provider

| File | Description | Used by |
|------|-------------|---------|
| `encovi_2023_c13sa_alimentos_3.parquet` | Food consumption module (ch. 13, sec. A, part 3) | M01 |
| `encovi_2023_hogares.parquet` | Household-level dataset | M01, M02 |
| `encovi_2023_personas.parquet` | Individual-level dataset | M01, M02 |
| `encovi_2023_c16sbc_produccion.parquet` | Agricultural production module (ch. 16, sec. B and C) | M02, M05 |
| `encovi_2023_c16sa_unidad.parquet` | Agricultural production unit (ch. 16, sec. A) | M02 |

### SIVESNU 2018 (Sistema de Vigilancia Epidemiológica de Salud y Nutrición)

**Provider:** SESAN/INCAP via SIINSAN
**Landing page:** https://portal.siinsan.gob.gt/monitoreo-y-evaluacion/
**Original format:** 4 Stata `.dta` files
**Redistribution:** not distributed here; download from the provider

| File | Description | Used by |
|------|-------------|---------|
| `sivesnu_gt18_hogar_publico.parquet` | Household-level survey | M04, M05 |
| `sivesnu_gt18_miembros_publico.parquet` | Household members survey | M04, M05 |
| `sivesnu_gt18_mujer_publico.parquet` | Women's health survey | M04 |
| `sivesnu_gt18_nino_publico.parquet` | Children's anthropometric survey | M04, M05 |

`raw_sources.yml` also lists the 2015 SIVESNU wave. Those four files
are marked inactive: they travel inside the provider's archive, `00_01`
does not unpack them, and the pipeline never reads them. They are not
in the hash log either, which covers the 22 active files only.

### INE Birth Registry (2019–2023)

**Provider:** Instituto Nacional de Estadística (INE), Guatemala
**Landing page:** https://www.ine.gob.gt/vitales/
**Original format:** 5 annual xlsx files
**Redistribution:** not distributed here; download from the provider

Script `00_02` unifies the five annual files into a single Parquet in
this directory:

| File | Description | Used by |
|------|-------------|---------|
| `ine_nacimientos_2019_2023.parquet` | Births registered 2019–2023 | M01 (01_03), for age estimation |

### WHO Child Growth Standards

**Provider:** World Health Organization
**Landing page:** https://www.who.int/tools/child-growth-standards
**Original format:** 8 xlsx files (boys and girls, by age band)
**Redistribution:** not distributed here; download from the provider

Script `00_02` unifies the eight files into three Parquet files:

| File | Description | Used by |
|------|-------------|---------|
| `who_standards_lhfa_monthly.parquet` | Length/height-for-age, monthly, 0–5 years | M05 |
| `who_standards_lhfa_daily.parquet` | Length/height-for-age, daily, 0–5 years | M05 |
| `who_reference_hfa_monthly.parquet` | Height-for-age reference, monthly, 5–19 years | M05 |

## Integrity Verification

`_hashes_active.csv` is one of the two files shipped with the
repository. It contains one row per original file — 22 in total,
covering the four active sources — with the SHA256 computed at download
time by `00_01`:

| Column | Content |
|--------|---------|
| `source_label` | Source group (`ENCOVI_2023`, `SIVESNU`, `INE_NACIMIENTOS`, `WHO_GROWTH`) |
| `filename` | Original file name as downloaded |
| `sha256_observed` | Hash computed from the downloaded file |
| `sha256_expected` | Hash declared in `raw_sources.yml` |
| `size_mb` | File size at download time |
| `status` | `match` when observed and expected agree |
| `recorded_on` | Date the hash was recorded |
| `citation` | Full citation for the source |

Because the hash log travels with the repository while the data does
not, it is the reference for anyone obtaining the sources
independently. To check a file, recompute its SHA256 and compare it
against the recorded value:

```r
source(here("02_code", "00_functions", "00_functions_provisioning.R"))

hash_log <- read_csv(here("01_data", "01_raw", "_hashes_active.csv"))
expected <- hash_log$sha256_expected[
  hash_log$filename == "encovi_2023_hogares.xlsx"
]

verify_hash_sha256("path/to/encovi_2023_hogares.xlsx", expected)
# returns status: "match" | "mismatch" | "missing"
```

A file whose SHA256 differs from `sha256_expected` is not the version
this analysis was built on. Note that after a complete run the
originals are no longer loose on disk: `00_02` archives them into
`_originals/active_raw_sources.zip` and removes them from the working
subdirectories, so the file to check is normally the one just
downloaded from the provider.

## Archive

`_originals/active_raw_sources.zip` contains the original Excel and
Stata files as downloaded, before Parquet conversion. It is created by
`00_02` after verifying conversion fidelity, and the originals are then
removed from the working subdirectories — which is why those
subdirectories hold only Parquet files after a complete run. The
archive is generated locally and is not distributed.

## Version Control and Reproduction

Nothing in this directory is versioned except this README and
`_hashes_active.csv`. The tree is excluded from Git in its entirety and
those two files are re-included by negation, because everything else is
obtainable from the official providers listed above.

To generate the contents, run Module 00 as shown under
[Provisioning](#provisioning). Module 00 is not part of the default
pipeline run: it is a one-time setup and audit task. To run the whole
project from raw-source provisioning, set `run_provisioning <- TRUE` in
`02_code/_run_all_project.R`.

The ENCOVI and SIVESNU microdata are published by their providers under
their own terms and are not redistributed in this repository. Obtain
them from the landing pages listed above, or let `00_01` download them
from the URLs recorded in `raw_sources.yml`. Without this directory
populated, Module 01 cannot run and neither can any module downstream
of it.
