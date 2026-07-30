# otargen

**Tidy R interface to the [Open Targets
Platform](https://platform.opentargets.org) GraphQL API.**

Query genes, diseases, drugs, variants, and genetic evidence directly
from R and receive analysis-ready tibbles — no manual JSON wrangling
required.

> **Python users:** see the sister package
> [otargenpy](https://github.com/amirfeizi/otargenpy).

------------------------------------------------------------------------

## Installation

**GitHub (recommended — latest features and bug fixes)**

``` r

# install.packages("devtools")
devtools::install_github("amirfeizi/otargen")
```

**CRAN (stable release)**

``` r

install.packages("otargen")
```

> **Tip:** The GitHub version often includes new functions and fixes
> ahead of the CRAN release. We recommend installing from GitHub to get
> the latest updates.

------------------------------------------------------------------------

## Quick start

Every function takes a single identifier (gene, disease, drug, or
variant) and returns a tidy tibble.

### Drug safety & mechanisms (by ChEMBL ID)

``` r

library(otargen)

# Adverse events reported for imatinib
adverseEventsQuery(chemblId = "CHEMBL941")

# Mechanism of action for imatinib
mechanismsOfActionQuery(chemblId = "CHEMBL941")

# Drug indications with clinical stage info
indicationsQuery(chemblId = "CHEMBL941")
```

### Gene-level queries (by Ensembl ID)

``` r

# Known drugs and clinical candidates targeting TP53
knownDrugsGeneQuery(ensgId = "ENSG00000141510")

# Cancer hallmarks for TP53
hallmarksQuery(ensgId = "ENSG00000141510")

# Protein-protein interactions for TP53 from IntAct
interactionsQuery(ensgId = "ENSG00000141510", sourceDatabase = "intact", size = 25)

# DepMap cancer cell-line essentiality for EGFR
depMapQuery(ensgId = "ENSG00000146648")

# Target safety liabilities for EGFR
safetyQuery(ensgId = "ENSG00000146648")

# Combined safety-liability score + rationale for EGFR
# (wraps geneticConstraintQuery, safetyQuery and depMapQuery)
targetLiability(ensgId = "ENSG00000146648")
```

### Gene + disease evidence (by Ensembl ID + EFO ID)

``` r

# ChEMBL evidence linking PARP1 to breast cancer
chemblQuery(ensemblId = "ENSG00000143799", efoId = "EFO_0000305")

# GWAS credible sets for PCSK9 and hyperlipidemia
gwasCredibleSetsQuery(ensemblId = "ENSG00000169174", efoId = "EFO_0004911")

# ClinVar evidence for BRCA1 and ovarian cancer
clinVarQuery(ensemblId = "ENSG00000012048", efoId = "EFO_0001075")

# Literature evidence from Europe PMC
europePMCQuery(ensemblId = "ENSG00000012048", efoId = "EFO_0001075")
```

### Pharmacogenomics & variants

``` r

# Pharmacogenomics data for a drug
pharmacogenomicsChemblQuery(chemblId = "CHEMBL1016")

# UniProt variants
uniProtVariantsQuery(variantId = "4_1804392_G_A")

# Variant effect predictions
variantEffectPredictorQuery(variantId = "1_154453788_C_T")
```

### Genetics & colocalisation

``` r

# Locus-to-gene predictions for a credible set
locus2GeneQuery(studyLocusId = "fa375739ca2a6b825ce5cc69d117e84b")

# GWAS colocalisation analysis
gwasColocalisation(study_locus_id = "5a86bfd40d2ebecf6ce97bbe8a737512")
```

------------------------------------------------------------------------

## Visualization

Built-in plotting functions turn query results into publication-ready
figures with a single call.

``` r

# Adverse events lollipop chart for imatinib
ae <- adverseEventsQuery(chemblId = "CHEMBL941")
plot_adverse_events(ae)

# Protein interaction network for TP53
int <- interactionsQuery(ensgId = "ENSG00000141510", sourceDatabase = "intact", size = 25)
plot_interactions(int)

# Locus-to-gene candidate ranking
l2g <- locus2GeneQuery(studyLocusId = "fa375739ca2a6b825ce5cc69d117e84b")
plot_l2g(l2g)

# GWAS colocalisation scatter plot
coloc <- gwasColocalisation(study_locus_id = "5a86bfd40d2ebecf6ce97bbe8a737512")
plot_colocalisation(coloc)

# Drug indications by clinical trial stage
ind <- indicationsQuery(chemblId = "CHEMBL941")
plot_indications(ind)
```

| Function | Input | Plot type |
|----|----|----|
| `plot_adverse_events` | `adverseEventsQuery` | Lollipop chart with significance threshold |
| `plot_interactions` | `interactionsQuery` | Circular network graph |
| `plot_l2g` | `locus2GeneQuery` | Ranked bar chart of L2G scores |
| `plot_colocalisation` | `gwasColocalisation` | H4 vs variant count scatter |
| `plot_indications` | `indicationsQuery` | Clinical stage bar chart |

------------------------------------------------------------------------

## Target prioritisation

[`targetLiability()`](https://amirfeizi.github.io/otargen/reference/targetLiability.md)
combines three existing queries — genetic constraint, known safety
liabilities, and DepMap essentiality — into a single 0–1 liability score
with a written rationale, flagging genes that look risky for full
inhibition versus ones that may tolerate a partial or tissue-restricted
approach.

``` r

# Score a single target (TP53) — prints a rationale and returns a tibble
targetLiability(ensgId = "ENSG00000141510")

# Skip the DepMap essentiality call (one fewer API request)
targetLiability(ensgId = "ENSG00000169174", include_essentiality = FALSE)

# Score and rank several targets
genes <- c("ENSG00000141510", "ENSG00000146648", "ENSG00000169174")
library(dplyr)
lapply(genes, function(g) targetLiability(g, verbose = FALSE)) |>
  bind_rows() |>
  arrange(desc(liability_score))
```

The result is one row per gene with:

| Column | Meaning |
|----|----|
| `liability_score` | Combined score, 0–1 (higher = greater liability) |
| `liability_category` | `Low`, `Moderate`, or `High` |
| `constraint_score`, `constraint_bin`, `constraint_loeuf` | Genetic constraint component (gnomAD LOEUF) |
| `n_safety_events`, `safety_score` | Known safety liabilities from [`safetyQuery()`](https://amirfeizi.github.io/otargen/reference/safetyQuery.md) |
| `essentiality_median`, `essentiality_score` | DepMap CRISPR gene-effect component |
| `recommendation`, `rationale` | Human-readable interpretation |

Scoring weights are genetic constraint 0.40, safety 0.35 and
essentiality 0.25, renormalised when a component has no data. It is a
heuristic triage aid, not a validated clinical safety measure.

------------------------------------------------------------------------

## Available functions (41)

| Category | Functions |
|----|----|
| **Drug queries** | `adverseEventsQuery`, `indicationsQuery`, `knownDrugsChemblQuery`, `mechanismsOfActionQuery`, `pharmacogenomicsChemblQuery` |
| **Gene / target queries** | `compGenomicsQuery`, `depMapQuery`, `geneOntologyQuery`, `geneticConstraintQuery`, `hallmarksQuery`, `interactionsQuery`, `knownDrugsGeneQuery`, `mousePhenotypesQuery`, `pathwaysQuery`, `pharmacogenomicsGeneQuery`, `safetyQuery` |
| **Target prioritisation** | `targetLiability` |
| **Gene + disease evidence** | `chemblQuery`, `clinVarQuery`, `europePMCQuery`, `geneBurdenQuery`, `genomicsEnglandQuery`, `orphanetQuery`, `uniprotLiteratureQuery` |
| **Variant queries** | `pharmacogenomicsVariantQuery`, `uniProtVariantsQuery`, `variantEffectPredictorQuery`, `variantEffectQuery`, `variantsQuery` |
| **Genetics / GWAS** | `gwasColocalisation`, `gwasCredibleSet`, `gwasCredibleSetsQuery`, `locus2GeneQuery`, `overlapInfoForStudy`, `qtlCredibleSetsQuery`, `sharedTraitStudiesQuery` |
| **Visualization** | `plot_adverse_events`, `plot_colocalisation`, `plot_indications`, `plot_interactions`, `plot_l2g` |

Full documentation: **<https://amirfeizi.github.io/otargen/>**

------------------------------------------------------------------------

## What’s new in 2.1.0

New
[`targetLiability()`](https://amirfeizi.github.io/otargen/reference/targetLiability.md)
helper that scores a target’s safety liability for inhibition or
knockout by combining
[`geneticConstraintQuery()`](https://amirfeizi.github.io/otargen/reference/geneticConstraintQuery.md),
[`safetyQuery()`](https://amirfeizi.github.io/otargen/reference/safetyQuery.md)
and
[`depMapQuery()`](https://amirfeizi.github.io/otargen/reference/depMapQuery.md).
Also fixes
[`gwasColocalisation()`](https://amirfeizi.github.io/otargen/reference/gwasColocalisation.md)
for single-row responses. See
[NEWS.md](https://amirfeizi.github.io/otargen/NEWS.md) for details.

------------------------------------------------------------------------

## Citation

If you use `otargen` in your research, please cite:

> Feizi A, Ray D (2023). otargen: an R package for accessing and
> visualizing Open Targets Genetics data. *Bioinformatics*, 39(7).
> <https://doi.org/10.1093/bioinformatics/btad441>

------------------------------------------------------------------------

## Contributing

Bug reports and feature requests: [GitHub
Issues](https://github.com/amirfeizi/otargen/issues)

Contributions are welcome via pull requests on
[GitHub](https://github.com/amirfeizi/otargen).

------------------------------------------------------------------------

## License

MIT
