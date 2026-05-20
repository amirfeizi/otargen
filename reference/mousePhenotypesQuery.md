# Retrieve Mouse Phenotypes data for a specified gene.

This function queries the Open Targets GraphQL API to retrieve mouse
phenotypes data for a specified gene.

## Usage

``` r
mousePhenotypesQuery(ensemblId)
```

## Arguments

- ensemblId:

  Character: ENSEMBL ID of the target gene (e.g., ENSG00000169174).

## Value

Returns a data frame containing mouse phenotypes data for the specified
gene.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- mousePhenotypesQuery(ensemblId = "ENSG00000169174")
} # }
```
