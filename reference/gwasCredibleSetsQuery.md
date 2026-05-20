# Retrieve GWAS Credible Sets data for a specified target and disease.

This function queries the Open Targets Platform GraphQL API to retrieve
GWAS credible sets evidence data for a specified target gene and
disease.

## Usage

``` r
gwasCredibleSetsQuery(ensemblId, efoId, size = 500)
```

## Arguments

- ensemblId:

  Character. Ensembl gene ID, e.g., "ENSG00000169174".

- efoId:

  Character. EFO disease ID, e.g., "EFO_0004911".

- size:

  Integer. Number of rows to fetch. Default: 500.

## Value

A tibble with credible set evidence or NULL if no data found.

## Examples

``` r
if (FALSE) { # \dontrun{
  result <- gwasCredibleSetsQuery(
    ensemblId = "ENSG00000169174",
    efoId = "EFO_0004911",
    size = 5
  )
  print(result)
} # }
```
