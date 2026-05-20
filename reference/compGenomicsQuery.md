# Retrieve Comparative Genomics data for a specified gene.

This function queries the Open Targets GraphQL API to retrieve
comparative genomics data for a specified gene.

## Usage

``` r
compGenomicsQuery(ensemblId)
```

## Arguments

- ensemblId:

  Character: ENSEMBL ID of the target gene (e.g., ENSG00000169174).

## Value

Returns a data frame containing comparative genomics data for the
specified gene.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- compGenomicsQuery(ensemblId = "ENSG00000169174")
} # }
```
