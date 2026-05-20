# examples

## Introduction

The **otargen** package provides seamless access to the Open Targets
Platform API, enabling researchers to query gene-disease associations,
variant annotations, pharmacogenomics, and more. This vignette
demonstrates examples from major functional areas in **otargen**,
organized into categories such as GWAS, genetic constraint analysis,
variants, pharmacogenomics, and more.

------------------------------------------------------------------------

## GWAS and Colocalisation

### Example: GWAS Credible Sets for Gene-Disease Associations

**Objective:** Identify causal variants linking a gene to a disease
using GWAS credible sets.  
**Function:** `gwasCredibleSetsQuery`

``` r

# Retrieve GWAS credible sets for APOE and Alzheimer’s disease
result <- gwasCredibleSetsQuery(
  ensemblId = "ENSG00000198125",
  efoId = "EFO_0003767",
  size = 5
)
print(result)
#> NULL
```

------------------------------------------------------------------------

## Genetic Analysis

### Example: Genetic Constraint Analysis

**Objective:** Assess a gene’s tolerance to mutations using genetic
constraint metrics like pLI and LOEUF.  
**Function:** `geneticConstraintQuery`

``` r

# Retrieve genetic constraint data for TP53
result <- geneticConstraintQuery(ensgId = "ENSG00000141510")
print(result)
#> # A tibble: 3 × 6
#>   constraintType score upperBin upperBin6 geneId          approvedSymbol
#>   <chr>          <dbl>    <int>     <int> <chr>           <chr>         
#> 1 syn            0.958       NA        NA ENSG00000141510 TP53          
#> 2 mis            1.15        NA        NA ENSG00000141510 TP53          
#> 3 lof            0.998        1         0 ENSG00000141510 TP53
```

------------------------------------------------------------------------

## Variants and Annotations

### Example: Clinical Variant Evidence

**Objective:** Explore ClinVar evidence for gene-disease associations to
uncover clinical significance.  
**Function:** `clinVarQuery`

``` r

# Retrieve ClinVar evidence for CFTR and cystic fibrosis
result <- clinVarQuery(
  ensemblId = "ENSG00000080815",
  efoId = "MONDO_0004975",
  size = 5
)
print(result)
#> # A tibble: 5 × 23
#>   directionOnTrait diseaseFromSource   variantRsId studyId clinicalSignificances
#>   <chr>            <chr>               <chr>       <chr>   <list>               
#> 1 risk             Alzheimer disease 3 rs63750083  RCV000… <chr [1]>            
#> 2 risk             Alzheimer disease 3 rs63751037  RCV000… <chr [1]>            
#> 3 risk             Alzheimer disease 3 rs63751024  RCV001… <chr [2]>            
#> 4 risk             Alzheimer disease 3 rs63750450  RCV001… <chr [2]>            
#> 5 risk             Alzheimer disease 3 rs63751287  RCV000… <chr [2]>            
#> # ℹ 18 more variables: allelicRequirements <list>, alleleOrigins <list>,
#> #   confidence <chr>, literature <list>, cohortPhenotypes <list>,
#> #   disease.id <chr>, disease.name <chr>, variant.id <chr>,
#> #   variant.hgvsId <chr>, variant.referenceAllele <chr>,
#> #   variant.alternateAllele <chr>, variantFunctionalConsequence.id <chr>,
#> #   variantFunctionalConsequence.label <chr>, approvedSymbol <chr>,
#> #   diseaseId <chr>, diseaseName <chr>, evaCount <int>, cursor <chr>
```

------------------------------------------------------------------------

### Example: UniProt Variants

**Objective:** Annotate variants with functional data from UniProt to
understand their biological impact.  
**Function:** `uniProtVariantsQuery`

``` r

# Retrieve UniProt variants for a specific variant
result <- uniProtVariantsQuery(variantId = "12_111446804_T_C")
print(result)
#> NULL
```

------------------------------------------------------------------------

## Pharmacogenomics

### Example: Pharmacogenomics Insights

**Objective:** Investigate how genetic variants influence drug response
using pharmacogenomics data.  
**Function:** `pharmacogenomicsQuery`

``` r

# Retrieve pharmacogenomics data for atorvastatin
result <- pharmacogenomicsChemblQuery(chemblId = "CHEMBL1016")
print(result)
#> # A tibble: 24 × 17
#>    variantRsId genotypeId       haplotypeId haplotypeFromSourceId isDirectTarget
#>    <chr>       <chr>            <lgl>       <lgl>                 <lgl>         
#>  1 rs3758785   11_94398973_A_A… NA          NA                    FALSE         
#>  2 rs3758785   11_94398973_A_G… NA          NA                    FALSE         
#>  3 rs6722745   2_108258788_T_T… NA          NA                    FALSE         
#>  4 rs1799998   8_142918184_A_G… NA          NA                    FALSE         
#>  5 rs3758785   11_94398973_A_A… NA          NA                    FALSE         
#>  6 rs6722745   2_108258788_T_C… NA          NA                    FALSE         
#>  7 rs5186      3_148742201_A_A… NA          NA                    TRUE          
#>  8 rs3184504   12_111446804_T_… NA          NA                    FALSE         
#>  9 rs740406    19_2232222_A_A,G NA          NA                    FALSE         
#> 10 rs740406    19_2232222_A_A,G NA          NA                    FALSE         
#> # ℹ 14 more rows
#> # ℹ 12 more variables: phenotypeFromSourceId <lgl>,
#> #   genotypeAnnotationText <chr>, phenotypeText <chr>, pgxCategory <chr>,
#> #   evidenceLevel <chr>, studyId <chr>, literature <list>,
#> #   variantFunctionalConsequence.id <chr>,
#> #   variantFunctionalConsequence.label <chr>, target.id <chr>,
#> #   target.approvedSymbol <chr>, drugId <chr>
```

------------------------------------------------------------------------

## Additional Functions and Categories

Beyond the examples above, **otargen** includes many other queries
grouped into logical categories:

- **Targets and Interactions**
  - compGenomicsQuery
  - depMapQuery
  - interactionsQuery
  - hallmarksQuery
  - mousePhenotypesQuery
  - safetyQuery
- **Pathways and Ontologies**
  - pathwaysQuery
  - geneOntologyQuery
- **External Data Sources**
  - europePMCQuery
  - orphanetQuery
  - genomicsEnglandQuery
- **Chembl and Drug Queries**
  - chemblQuery
  - indicationsQuery

See the package reference documentation for details on these functions
and parameters.

------------------------------------------------------------------------

## Learn More

For a full list of available queries and example usage:

``` r

help(package = "otargen")
```

or browse the function reference on the pkgdown site.

------------------------------------------------------------------------
