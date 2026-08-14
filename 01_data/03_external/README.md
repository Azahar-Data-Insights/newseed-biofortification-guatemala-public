# External Data Directory

Contains expert-compiled reference data that is not available from
public download URLs and cannot be reproduced by the Module 00
provisioning scripts. This directory is committed to version control.

## Contents

```
03_external/
├── incap_finut_master.xlsx          # Editable source of truth (3 data sheets + README)
├── incap_food_composition.parquet   # Built from the master (one row per food)
└── incap_food_equivalences.parquet  # Built from the master (one row per food × unit)
```

The two Parquet files are **generated artifacts**, not hand-edited
sources. They are produced from `incap_finut_master.xlsx` by the
Module 00 build script (`00_03_build_nutrition_tables.qmd`) and are
the files actually read by the pipeline (Script 01_01). To change any
value, edit the master workbook and re-run the build; never edit the
Parquet files directly.

## Source of truth: `incap_finut_master.xlsx`

A single editable workbook with four sheets:

| Sheet | Granularity | Content |
|-------|-------------|---------|
| `README` | — | Purpose, editing protocol, lineage, auditor notes |
| `food_composition` | One row per food (`codine`) | Energy, protein, iron, zinc, edible-portion factor, amino acids, PDCAAS, NPU, QPM values |
| `food_equivalences` | One row per food × presentation unit | Local unit-to-grams conversions and plant-based classification |
| `source_notes` | One row per documented value | Per-value provenance for PDCAAS / NPU / QPM scores |

### Lineage

The data has three provenance layers, documented per value in the
workbook's `source_notes` sheet:

1. **INCAP composition base.** Energy, protein, iron, zinc and the
   edible-portion factor (`pct_aprov`) were transcribed from an INCAP
   food composition report for Central American foods. Foods are keyed
   by the INCAP food code (`ali`) and the ENCOVI survey food code
   (`codine`).

2. **FINUT protein-quality layer.** Amino acid profiles, PDCAAS and
   NPU starting values were compiled by María José Soto (FINUT) from
   the literature — principally Boye et al. (2012), Suárez López et
   al. (2006) and FAO amino acid data tables — following the FAO/WHO
   PDCAAS methodology (WHO/FAO/UNU 2007). Quality Protein Maize (QPM)
   values correspond to variety V-537 (Boye et al. 2012).

3. **Azahar Data Insights derived layer.** Where literature values
   were unavailable for a food, Azahar Data Insights derived or imputed
   scores from comparable food groups (legumes, citrus, tropical
   fruits, pome and stone fruits, soups, or the maize and soybean base
   values). These derivations are individually flagged in
   `source_notes`.

The food equivalence table (unit-to-grams conversions and plant-based
classification) was compiled by María José Soto (FINUT) through
on-site market research in Guatemala (2024), reflecting actual local
retail packaging rather than theoretical standards.

## Usage in the pipeline

Script 01_01 (Module 01) reads the two Parquet tables to build the
integrated food composition database (`01_01_incap_integrated.parquet`)
and the food equivalence table (`01_01_food_equivalences.parquet`),
which feed household intake calculation in Script 01_02. The join key
to ENCOVI consumption data is `codine`.

| Built file | Consumed by | Purpose |
|------------|-------------|---------|
| `incap_food_composition.parquet` | M01 (01_01) | Nutrient composition per food; protein quality assessment |
| `incap_food_equivalences.parquet` | M01 (01_01) | Unit-to-grams conversion; plant-based phytate classification |

## Data quality notes

- Six consumable items carry no INCAP food code and therefore no
  composition (water, beer, spirits, cigarettes, other packaged items,
  baby compotes). They appear in `food_equivalences` but not in
  `food_composition`, and are excluded from nutrient calculations by
  design.
- One food has no PDCAAS value; Script 01_01 assigns a conservative
  0.1 (incomplete-protein assumption).
- In `food_equivalences`, `gramos = 0` marks an unavailable unit
  conversion for that food × unit combination. A small number of rows
  carry no numeric measurement code because the unit label has no
  leading number; these do not match the consumption join and are
  inert.
- The INCAP food code (`ali`) may repeat across rows; the survey food
  code (`codine`) is unique and is the operative join key.

## Version control

This entire directory is committed to Git. Files are small and cannot
be reproduced from public sources.

## License

The AGPL-3.0 license applies to the code in this repository. The
nutrition tables in this directory are distributed to make the analysis
reproducible. The food composition base is transcribed from published
INCAP sources; the protein-quality layer was compiled by FINUT from the
literature cited below. Reuse of these tables should cite the original
sources rather than this repository.

## References

- Boye, J., Wijesinha-Bettoni, R., & Burlingame, B. (2012). Protein
  quality evaluation twenty years after the introduction of the PDCAAS
  method. *British Journal of Nutrition*, 108(S2), S183–S211.
- Suárez López, M. M., Kizlansky, A., & López, L. B. (2006).
  Evaluación de la calidad de las proteínas en los alimentos calculando
  el escore de aminoácidos corregido por digestibilidad. *Nutrición
  Hospitalaria*, 21(1), 47–51.
- Rutherfurd, S. M., Fanning, A. C., Miller, B. J., & Moughan, P. J.
  (2015). PDCAAS and DIAAS differentially describe protein quality in
  growing male rats. *Journal of Nutrition*, 145(2), 372–379.
- WHO/FAO/UNU (2007). *Protein and Amino Acid Requirements in Human
  Nutrition*. WHO Technical Report Series 935.
- INCAP (2012). *Tabla de Composición de Alimentos de Centroamérica*.
  Guatemala: INCAP.

## Contact

- Nutrition expert: María José Soto (FINUT — Fundación Iberoamericana
  de Nutrición)
- Project technical lead: Azahar Data Insights (info@azahardata.com)
