# Retrieve ClinVar data for a specified gene and disease.

This function queries the Open Targets GraphQL API to retrieve ClinVar
evidence data for a specified gene and disease.

## Usage

``` r
clinVarQuery(ensemblId, efoId, cursor = NULL, size = 10)
```

## Arguments

- ensemblId:

  Character: ENSEMBL ID of the target gene (e.g., "ENSG00000080815").

- efoId:

  Character: EFO ID of the target disease (e.g., "MONDO_0004975").

- cursor:

  Character: Cursor for pagination (default: NULL).

- size:

  Integer: Number of records to retrieve (default: 10).

## Value

Returns a tibble containing ClinVar evidence data for the specified gene
and disease.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- clinVarQuery(ensemblId = "ENSG00000080815", efoId = 
"MONDO_0004975", size = 10)
result <- clinVarQuery(ensemblId = "ENSG00000080815", efoId = 
"MONDO_0004975", cursor = NULL, size = 10)
} # }
```
