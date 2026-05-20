# Retrieve Pharmacogenomics data for a specified gene.

This function queries the Open Targets GraphQL API to retrieve
pharmacogenomics data for a specified gene.

## Usage

``` r
pharmacogenomicsGeneQuery(ensgId)
```

## Arguments

- ensgId:

  Character: ENSEMBL ID of the target gene (e.g., ENSG00000141510).

## Value

Returns a tibble containing pharmacogenomics data for the specified
gene.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- pharmacogenomicsGeneQuery(ensgId = "ENSG00000141510")
} # }
```
