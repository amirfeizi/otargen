# Retrieve Known Drugs data for a specified drug.

This function queries the Open Targets GraphQL API to retrieve known
drugs data for a specified drug.

## Usage

``` r
knownDrugsChemblQuery(chemblId, cursor = NULL, freeTextQuery = NULL, size = 10)
```

## Arguments

- chemblId:

  Character: ChEMBL ID of the target drug (e.g., "CHEMBL1016").

- cursor:

  Character: Cursor for pagination (default: NULL).

- freeTextQuery:

  Character: Free text query to filter results (default: NULL).

- size:

  Integer: Number of records to retrieve (default: 10).

## Value

Returns a tibble containing known drugs data for the specified drug.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- knownDrugsChemblQuery(chemblId = "CHEMBL1016", size = 10)
result <- knownDrugsChemblQuery(chemblId = "CHEMBL1016", cursor = NULL, 
freeTextQuery = NULL, size = 10)
} # }
```
