# Retrieve Europe PMC data for a specified gene and disease.

This function queries the Open Targets GraphQL API to retrieve Europe
PMC evidence data for a specified gene and disease.

## Usage

``` r
europePMCQuery(ensemblId, efoId, cursor = NULL, size = 50)
```

## Arguments

- ensemblId:

  Character: ENSEMBL ID of the target gene (e.g., "ENSG00000080815").

- efoId:

  Character: EFO ID of the target disease (e.g., "MONDO_0004975").

- cursor:

  Character: Cursor for pagination (default: NULL).

- size:

  Integer: Number of records to retrieve (default: 50).

## Value

Returns a tibble containing Europe PMC evidence data for the specified
gene and disease.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- europePMCQuery(ensemblId = "ENSG00000080815",
 efoId = "MONDO_0004975", size = 50)
result <- europePMCQuery(ensemblId = "ENSG00000080815", 
efoId = "MONDO_0004975", cursor = NULL, size = 50)
} # }
```
