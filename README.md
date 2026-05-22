<!-- badges -->
[![CRAN status](https://www.r-pkg.org/badges/version/otargen)](https://CRAN.R-project.org/package=otargen)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/otargen)](https://CRAN.R-project.org/package=otargen)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)

# otargen

**Tidy R interface to the [Open Targets Platform](https://platform.opentargets.org) GraphQL API.**

Query genes, diseases, drugs, variants, and genetic evidence directly from R and receive
analysis-ready tibbles — no manual JSON wrangling required.

---

## Installation

**CRAN (stable)**

```r
install.packages("otargen")
```

**GitHub (development)**

```r
# install.packages("devtools")
devtools::install_github("amirfeizi/otargen")
```

---

## Quick start

Every function takes a single identifier (gene, disease, drug, or variant) and returns a tidy tibble.

### Drug safety & mechanisms (by ChEMBL ID)

```r
library(otargen)

# Adverse events reported for imatinib
adverseEventsQuery(chemblId = "CHEMBL941")

# Mechanism of action for imatinib
mechanismsOfActionQuery(chemblId = "CHEMBL941")

# Drug indications with clinical stage info
indicationsQuery(chemblId = "CHEMBL941")
```

### Gene-level queries (by Ensembl ID)

```r
# Known drugs and clinical candidates targeting TP53
knownDrugsGeneQuery(ensgId = "ENSG00000141510")

# Cancer hallmarks for TP53
hallmarksQuery(ensgId = "ENSG00000141510")

# Protein-protein interactions for TP53
interactionsQuery(ensgId = "ENSG00000141510", size = 25)

# DepMap cancer cell-line essentiality for EGFR
depMapQuery(ensgId = "ENSG00000146648")

# Target safety liabilities for EGFR
safetyQuery(ensgId = "ENSG00000146648")
```

### Gene + disease evidence (by Ensembl ID + EFO ID)

```r
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

```r
# Pharmacogenomics data for a drug
pharmacogenomicsChemblQuery(chemblId = "CHEMBL1016")

# UniProt variants
uniProtVariantsQuery(variantId = "4_1804392_G_A")

# Variant effect predictions
variantEffectPredictorQuery(variantId = "1_154453788_C_T")
```

### Genetics & colocalisation

```r
# Locus-to-gene predictions for a credible set
locus2GeneQuery(studyLocusId = "fa375739ca2a6b825ce5cc69d117e84b")

# GWAS colocalisation analysis
gwasColocalisation(study_locus_id = "5a86bfd40d2ebecf6ce97bbe8a737512")
```

---

## Available functions (35)

| Category | Functions |
|---|---|
| **Drug queries** | `adverseEventsQuery`, `indicationsQuery`, `knownDrugsChemblQuery`, `mechanismsOfActionQuery`, `pharmacogenomicsChemblQuery` |
| **Gene / target queries** | `compGenomicsQuery`, `depMapQuery`, `geneOntologyQuery`, `geneticConstraintQuery`, `hallmarksQuery`, `interactionsQuery`, `knownDrugsGeneQuery`, `mousePhenotypesQuery`, `pathwaysQuery`, `pharmacogenomicsGeneQuery`, `safetyQuery` |
| **Gene + disease evidence** | `chemblQuery`, `clinVarQuery`, `europePMCQuery`, `geneBurdenQuery`, `genomicsEnglandQuery`, `orphanetQuery`, `uniprotLiteratureQuery` |
| **Variant queries** | `pharmacogenomicsVariantQuery`, `uniProtVariantsQuery`, `variantEffectPredictorQuery`, `variantEffectQuery`, `variantsQuery` |
| **Genetics / GWAS** | `gwasColocalisation`, `gwasCredibleSet`, `gwasCredibleSetsQuery`, `locus2GeneQuery`, `overlapInfoForStudy`, `qtlCredibleSetsQuery`, `sharedTraitStudiesQuery` |

Full documentation: **<https://amirfeizi.github.io/otargen/>**

---

## What's new in 2.0.1

Bug fixes for HTTP 400 errors caused by upstream Open Targets API schema changes.
Nine functions updated: `chemblQuery`, `clinVarQuery`, `geneBurdenQuery`, `orphanetQuery`,
`indicationsQuery`, `knownDrugsChemblQuery`, `knownDrugsGeneQuery`, `geneOntologyQuery`,
and `interactionsQuery`. See [NEWS.md](NEWS.md) for details.

---

## Citation

If you use `otargen` in your research, please cite:

> Feizi A, Ray D (2023). otargen: an R package for accessing
> and visualizing Open Targets Genetics data. *Bioinformatics*, 39(7).
> <https://doi.org/10.1093/bioinformatics/btad441>

---

## Contributing

Bug reports and feature requests: [GitHub Issues](https://github.com/amirfeizi/otargen/issues)

Contributions are welcome via pull requests on [GitHub](https://github.com/amirfeizi/otargen).

---

## License

MIT
