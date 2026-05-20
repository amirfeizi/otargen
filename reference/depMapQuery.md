# Retrieve DepMap Essentiality data for a specified gene.

This function queries the Open Targets GraphQL API to retrieve DepMap
essentiality data for a specified gene.

## Usage

``` r
depMapQuery(ensgId)
```

## Arguments

- ensgId:

  Character: ENSEMBL ID of the target gene (e.g., ENSG00000141510).

## Value

Returns a tibble containing DepMap essentiality data for the specified
gene.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- depMapQuery(ensgId = "ENSG00000141510")
} # }
```
