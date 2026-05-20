# Retrieve Known Drugs data for a specified gene.

This function queries the Open Targets GraphQL API to retrieve known
drugs data for a specified gene.

## Usage

``` r
knownDrugsGeneQuery(ensgId, cursor = NULL, freeTextQuery = NULL, size = 10)
```

## Arguments

- ensgId:

  Character: ENSEMBL ID of the target gene (e.g., ENSG00000169174).

- cursor:

  Character: Cursor for pagination (default: NULL).

- freeTextQuery:

  Character: Free text query to filter results (default: NULL).

- size:

  Integer: Number of records to retrieve (default: 10).

## Value

Returns a data frame containing known drugs data for the specified gene.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- knownDrugsGeneQuery(ensgId = "ENSG00000169174", size = 10)
result <- knownDrugsGeneQuery(ensgId = "ENSG00000169174",
 cursor = NULL, freeTextQuery = NULL, size = 10)
} # }
```
