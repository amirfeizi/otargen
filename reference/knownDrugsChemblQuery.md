# Retrieve indications and clinical report data for a specified drug.

This function queries the Open Targets GraphQL API to retrieve
indications and clinical report data for a specified drug.

## Usage

``` r
knownDrugsChemblQuery(chemblId)
```

## Arguments

- chemblId:

  Character: ChEMBL ID of the target drug (e.g., "CHEMBL1016").

## Value

Returns a tibble containing indications data for the specified drug.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- knownDrugsChemblQuery(chemblId = "CHEMBL1016")
} # }
```
