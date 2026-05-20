# Retrieve Indications data for a specified drug.

This function queries the Open Targets GraphQL API to retrieve
indications data for a specified drug.

## Usage

``` r
indicationsQuery(chemblId)
```

## Arguments

- chemblId:

  Character: ChEMBL ID of the target drug (e.g., "CHEMBL1016").

## Value

Returns a tibble containing indications data for the specified drug.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- indicationsQuery(chemblId = "CHEMBL1016")
} # }
```
