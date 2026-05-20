# Retrieve Cancer Hallmarks data for a specified gene.

This function queries the Open Targets GraphQL API to retrieve cancer
hallmarks data for a specified gene.

## Usage

``` r
hallmarksQuery(ensgId)
```

## Arguments

- ensgId:

  Character: ENSEMBL ID of the target gene (e.g., ENSG00000141510).

## Value

Returns a tibble containing cancer hallmarks data for the specified
gene.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- hallmarksQuery(ensgId = "ENSG00000141510")
} # }
```
