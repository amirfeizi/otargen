# Retrieve Pathways data for a specified gene.

This function queries the Open Targets GraphQL API to retrieve pathways
data for a specified gene.

## Usage

``` r
pathwaysQuery(ensgId)
```

## Arguments

- ensgId:

  Character: ENSEMBL ID of the target gene (e.g., ENSG00000105397).

## Value

Returns a tibble containing pathways data for the specified gene.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- pathwaysQuery(ensgId = "ENSG00000105397")
} # }
```
