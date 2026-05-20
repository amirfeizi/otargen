# Retrieve Genetic Constraint data for a specified gene.

This function queries the Open Targets Platform GraphQL API to retrieve
genetic constraint data for a specified gene, such as pLI or LOEUF
scores.

## Usage

``` r
geneticConstraintQuery(ensgId)
```

## Arguments

- ensgId:

  Character: ENSEMBL ID of the target gene (e.g., "ENSG00000141510").

## Value

Returns a tibble containing genetic constraint data for the specified
gene.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- geneticConstraintQuery(ensgId = "ENSG00000141510")
} # }
```
