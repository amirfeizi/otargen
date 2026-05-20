# Retrieve Mechanisms of Action data for a specified drug.

This function queries the Open Targets GraphQL API to retrieve
mechanisms of action data for a specified drug.

## Usage

``` r
mechanismsOfActionQuery(chemblId)
```

## Arguments

- chemblId:

  Character: ChEMBL ID of the target drug (e.g., "CHEMBL1016").

## Value

Returns a tibble containing mechanisms of action data for the specified
drug.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- mechanismsOfActionQuery(chemblId = "CHEMBL1016")
} # }
```
