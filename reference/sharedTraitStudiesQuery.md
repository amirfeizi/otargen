# Retrieve Shared Trait Studies data for specified diseases.

This function queries the Open Targets GraphQL API to retrieve shared
trait studies data for specified disease IDs.

## Usage

``` r
sharedTraitStudiesQuery(diseaseIds, size = 500, index = 0)
```

## Arguments

- diseaseIds:

  Character vector: IDs of the target diseases (e.g., c("EFO_0004587")).

- size:

  Integer: Number of records to retrieve (default: 500).

- index:

  Integer: Page index for pagination (default: 0).

## Value

Returns a tibble containing shared trait studies data for the specified
diseases.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- sharedTraitStudiesQuery(diseaseIds = c("EFO_0004587"), size = 500, 
index = 0)
} # }
```
