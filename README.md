# Biofortified Maize Impact Assessment Framework for Guatemala

[![License: AGPL v3](https://img.shields.io/badge/License-AGPL_v3-blue.svg)](https://www.gnu.org/licenses/agpl-3.0)

An analytical framework for evaluating the nutritional and economic impact of biofortified maize adoption on child stunting in Guatemala. Developed by [Azahar Data Insights](https://azahardata.com/) for [New Seed (Semilla Nueva)](https://www.semillanueva.org/), with nutritional methodology advisory from [FINUT](https://www.finut.org/).

---

## Overview

Guatemala has one of the highest rates of chronic child malnutrition (stunting) worldwide, affecting physical and cognitive development with lifelong consequences for health, education, and productivity. Maize is the central staple food in the Guatemalan diet, representing both a nutritional challenge and a key opportunity for large-scale improvement.

New Seed has developed biofortified Quality Protein Maize (QPM) varieties with higher iron, higher zinc, and an improved amino acid profile (lysine and tryptophan). This framework provides a quantitative basis for estimating how different levels of biofortified maize adoption could relate to stunting prevalence across Guatemala's departments.

The framework integrates several national data sources to model the pathway from agricultural adoption to nutritional outcomes, supporting evidence-based scaling strategies and donor engagement. All impacts reported here are **modeled estimates** derived from observational survey data, not measured outcomes of a deployed intervention.

---

## Methodology

The analytical pipeline is organized into six sequential modules, preceded by a one-time data-provisioning module. Each module consumes the outputs of the previous ones.

**Module 01 — Nutrient Intakes**
Estimates individual-level nutrient intake from household consumption data using the Adult Male Equivalent (AME) methodology. Maize-derived nutrients are separated from other dietary sources to enable substitution modeling.

**Module 02 — Economic Impact**
Segments farmers by current seed technology, calibrates agricultural weights against official MAGA statistics, and models the economic impact of biofortified adoption by segment. A likely-adopter model estimates adoption probability under different seed and subsidy regimes.

**Module 03 — Transfer Models**
Develops survey-weighted gamma regression models to transfer nutrient intake profiles from ENCOVI (consumption survey) to SIVESNU (anthropometric survey) using shared sociodemographic predictors. Covers iron, zinc, digestible protein, lysine, tryptophan, energy, and plant-based food percentage.

**Module 04 — Nutritional Impact (Covariate Prediction)**
Prepares SIVESNU child data, integrates Chispitas micronutrient supplementation and bioavailability adjustments, and fits a survey-weighted continuous height-for-age (HAZ) model. A synthetic child population is generated to support scenario precomputation.

**Module 05 — Nutritional Impact (Meta-Analysis)**
Applies a published meta-analysis effect size (Gunaratna et al., 2010) for QPM consumption to the ENCOVI child population through a height transfer model, providing an independent estimate of stunting impact.

**Module 06 — Scenario Precomputation**
Combines the upstream modules to precompute departmental and national scenarios across production coverage levels, seed varieties, subsidy regimes, and both impact pathways, including a gravity-based production–consumption redistribution model.

The two impact pathways (Covariate prediction and Meta-analysis) are reported side by side so that estimates can be compared across methodologies.

📖 [Methodological Documentation](https://newseed.azahardata.com/web-guatemala) — Complete model specifications, distribution choices, and validation results.

---

## Results

The interactive dashboard enables exploration of biofortification scenarios across multiple dimensions:

- **Production coverage scenarios**: modeled estimates from 0% to 100% adoption in 10-point increments
- **Geographic targeting**: department-level projections accounting for production–consumption balance
- **Seed and subsidy regimes**: outcomes by seed variety and subsidy scenario
- **Farmer segmentation**: economic impact by seed-technology segment (non-hybrid OPV/Criollo, Low, Mid, High)
- **Nutritional outcomes**: projected changes in stunting prevalence and height-for-age z-score distributions
- **Stunting methodology**: results under both the Covariate prediction and Meta-analysis (Gunaratna) pathways

🌐 [Interactive Dashboard](https://newseed.azahardata.com/app-guatemala) — Explore scenarios and download results.

---

## Repository Structure

```
├── .github/
│   ├── CONTRIBUTING.md
│   ├── CODE_OF_CONDUCT.md
│   ├── SECURITY.md
│   └── SUPPORT.md
│
├── 01_data/
│   ├── 01_raw/                  # Official survey data (not included — see Data Sources)
│   ├── 02_processed/            # Pipeline-generated data (not included — reproducible)
│   │   ├── transfer/            #   Inter-script datasets
│   │   ├── models/              #   Trained model objects (.rds)
│   │   └── scenarios/           #   Pre-computed scenario data for the dashboard
│   └── 03_external/             # INCAP/FINUT nutrition tables (included)
│
├── 02_code/
│   ├── _config/                 # Single-source-of-truth YAML variant files
│   ├── 00_functions/            # Shared utility functions
│   ├── 00_data_provisioning/    # One-time raw-source download and conversion
│   ├── 01_nutritional_intakes/
│   ├── 02_economic_impact/
│   ├── 03_transfer_models/
│   ├── 04_nutritional_impact_covariate/
│   ├── 05_nutritional_impact_meta_analysis/
│   ├── 06_scenario_precomputation/
│   └── _run_all_project.R       # Top-level pipeline orchestrator
│
├── 03_methodological_web/       # Quarto sources of the methodological site
│   └── references.bib           # Bibliography for sources and methods
│
├── renv/                        # R environment for reproducibility
├── renv.lock                    # Package versions lockfile
├── .zenodo.json                 # Zenodo metadata for DOI
├── CITATION.cff                 # Citation metadata
├── LICENSE                      # AGPL-3.0
└── README.md                    # This file
```

The pipeline is launched through orchestrators (`_run_all_*.R`): module orchestrators run steps in dependency order, and parametric sub-orchestrators expand seed, subsidy, pathway, and nutrient variants defined in `02_code/_config/`. See [`02_code/README.md`](02_code/README.md) for the full execution model.

---

## Data Sources

| Source | Year | Description | Access |
|--------|------|-------------|--------|
| ENCOVI | 2023 | National Living Conditions Survey — household consumption, socioeconomic characteristics, agricultural production | [INE Guatemala](https://www.ine.gob.gt/pobreza-menu/) |
| SIVESNU | 2018 | Nutrition Surveillance System — child anthropometry, maternal characteristics, health indicators | [SIINSAN](https://portal.siinsan.gob.gt/monitoreo-y-evaluacion/) |
| MAGA | 2023 | Official agricultural statistics — departmental basic grains production | [Production report (PDF)](https://precios.maga.gob.gt/archivos/produccion/Informe%20de%20Producci%C3%B3n%20de%20Granos%20B%C3%A1sicos%20Diciembre%202023.pdf) |
| INCAP / FINUT | — | Food composition tables, PDCAAS values, amino acid profiles, local unit equivalences | Included in repository |
| WHO Child Growth Standards | — | Length/height-for-age standards (0–19 years) for z-score computation | [who.int](https://www.who.int/tools/child-growth-standards) |
| ENSMI | 2014–15 | Maternal and Child Health Survey — stunting prevalence benchmarks | [DHS Final Report (PDF)](https://www.dhsprogram.com/pubs/pdf/FR318/FR318.pdf) |
| MSPAS | 2023 | Chispitas supplementation coverage by department | [Open data portal](https://datosabiertos.mspas.gob.gt/dataset/suplementacion-en-ninos-menores-5-ano-2013) |

Raw microdata from ENCOVI and SIVESNU require formal data access requests and are **not** included in this repository. The expert-compiled INCAP/FINUT nutrition tables in `01_data/03_external/` are included. Intermediate and final processed files are reproducible by running the pipeline. See the README files inside `01_data/` for provenance and access details.

Full bibliographic references for data sources and methodological decisions are provided in [`03_methodological_web/references.bib`](03_methodological_web/references.bib).

---

## Getting Started

### Requirements

- R ≥ 4.6.0
- [Positron](https://positron.posit.co/) or any IDE with Quarto support
- Quarto ≥ 1.9
- [renv](https://rstudio.github.io/renv/) for dependency management

### Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/Azahar-Data-Insights/newseed-biofortification-guatemala-public.git
   cd newseed-biofortification-guatemala-public
   ```

2. Restore the R environment:
   ```r
   install.packages("renv")
   renv::restore()
   ```

### Execution

The full analytical pipeline (Modules 01–06) runs end to end with a single call to the project orchestrator:

```r
source(here::here("02_code", "_run_all_project.R"))
```

Module 00 (data provisioning) is not part of the default run; it downloads and converts official raw sources and is intended for a fresh environment or a full reproducibility audit. Individual modules and parametric steps can also be run on their own, in dependency order. See [`02_code/README.md`](02_code/README.md) for details.

Module 01 requires raw ENCOVI/SIVESNU microdata, which are not distributed here. Without access to them the pipeline cannot be run end to end, but the code, the parameters in `02_code/_config/` and the methodological sources in `03_methodological_web/` document every step, and the rendered site reports the results of the full run.

---

## Team

### Development

- **Julia María Sánchez Tormo** — Senior Data Scientist, Azahar Data Insights
  [ORCID](https://orcid.org/0000-0001-9341-8737)

- **Rubén Palomo Llinares** — Senior Data Scientist, Azahar Data Insights
  [ORCID](https://orcid.org/0000-0002-1890-4337)

### Methodological Advisory

- **María José Soto Méndez, Ph.D.** — Scientific Director, FINUT
  [ORCID](https://orcid.org/0000-0002-1012-4715)

- **Katherine P. Adams, Ph.D.** — Director of Impact and Cost Effectiveness, New Seed
  [ORCID](https://orcid.org/0000-0002-1060-2473)

### Project Direction

- **Curt Bowen** — Co-Founder & Executive Director, New Seed

---

## Citation

If you use this framework in your research, please cite it using the metadata in [CITATION.cff](CITATION.cff), or the following:

```bibtex
@software{sanchez_tormo_biofortification_guatemala,
  author    = {Sánchez Tormo, Julia María and
               Palomo Llinares, Rubén and
               Soto Méndez, María José and
               Adams, Katherine P. and
               Bowen, Curt},
  title     = {{Biofortified Maize Impact Assessment Framework
                for Guatemala}},
  year      = {2026},
  version   = {4.0.0},
  license   = {AGPL-3.0},
  url       = {https://github.com/Azahar-Data-Insights/newseed-biofortification-guatemala-public}
}
```

---

## License

This project is licensed under the GNU Affero General Public License v3.0 (AGPL-3.0).

You are free to use, modify, and distribute this code, provided that you:

- maintain the same license for derivative works,
- make source code available if you deploy modified versions as a network service,
- preserve copyright notices and attribution.

See [LICENSE](LICENSE) for the full text.

The AGPL-3.0 license applies to the code in this repository. The nutrition tables in `01_data/03_external/` are distributed to make the analysis reproducible. The food composition base is transcribed from published INCAP sources; the protein-quality layer was compiled by FINUT from the literature cited in that directory's README. Reuse of these tables should cite the original sources rather than this repository.

The methodological website in `03_methodological_web/` is part of this release: its Quarto sources are published here as auditable material, and the rendered site is available at [newseed.azahardata.com/web-guatemala](https://newseed.azahardata.com/web-guatemala). The interactive Shiny dashboard is a separate component developed by Azahar Data Insights and its source code is not part of this open-source release; all the figures it displays are precomputed by Module 06 and included here, so its absence does not affect the reproducibility of any reported result.

---

## Acknowledgments

This work was commissioned and funded by New Seed (Semilla Nueva), with nutritional methodology advisory from FINUT. We gratefully acknowledge the institutions that provided data access. See [ACKNOWLEDGMENTS.md](ACKNOWLEDGMENTS.md) and [FUNDERS.md](FUNDERS.md) for details.

---

## Contact

- Technical inquiries: [info@azahardata.com](mailto:info@azahardata.com)
- Biofortified maize program: [info@semillanueva.org](mailto:info@semillanueva.org)

For the project's support and contribution policy, see [SUPPORT.md](.github/SUPPORT.md) and [CONTRIBUTING.md](.github/CONTRIBUTING.md).
