# Retrieve drug and clinical candidate data for a specified gene.

This function queries the Open Targets GraphQL API to retrieve drug and
clinical candidate data for a specified gene.

## Usage

``` r
knownDrugsGeneQuery(ensgId)
```

## Arguments

- ensgId:

  Character: ENSEMBL ID of the target gene (e.g., ENSG00000169174).

## Value

Returns a data frame containing drug and clinical candidate data for the
specified gene.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- knownDrugsGeneQuery(ensgId = "ENSG00000169174")
} # }
```
