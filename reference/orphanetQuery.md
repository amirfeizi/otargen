# Retrieve Orphanet data for a specified gene and disease.

This function queries the Open Targets GraphQL API to retrieve Orphanet
evidence data for a specified gene and disease.

## Usage

``` r
orphanetQuery(ensemblId, efoId, size = 3500)
```

## Arguments

- ensemblId:

  Character: ENSEMBL ID of the target gene (e.g., "ENSG00000080815").

- efoId:

  Character: EFO ID of the target disease (e.g., "MONDO_0004975").

- size:

  Integer: Number of records to retrieve (default: 3500).

## Value

Returns a tibble containing Orphanet evidence data for the specified
gene and disease.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- orphanetQuery(ensemblId = "ENSG00000080815", efoId = 
"MONDO_0004975", size = 3500)
} # }
```
