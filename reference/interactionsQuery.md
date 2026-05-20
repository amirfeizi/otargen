# Retrieve Interactions data for a specified gene.

This function queries the Open Targets GraphQL API to retrieve molecular
interaction data for a specified gene.

## Usage

``` r
interactionsQuery(ensgId, sourceDatabase = NULL, index = 0, size = 10)
```

## Arguments

- ensgId:

  Character: ENSEMBL ID of the target gene (e.g., ENSG00000141510).

- sourceDatabase:

  Character: Source database for interactions (e.g., "intact") (default:
  NULL).

- index:

  Integer: Page index for pagination (default: 0).

- size:

  Integer: Number of records to retrieve (default: 10).

## Value

Returns a tibble containing interactions data for the specified gene.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- interactionsQuery(ensgId = "ENSG00000141510",
 sourceDatabase = "intact", size = 10)
result <- interactionsQuery(ensgId = "ENSG00000141510",
 sourceDatabase = "intact", index = 0, size = 10)
} # }
```
