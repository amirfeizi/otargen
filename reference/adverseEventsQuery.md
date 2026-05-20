# Retrieve Adverse Events data for a specified drug.

This function queries the Open Targets GraphQL API to retrieve adverse
events data for a specified drug.

## Usage

``` r
adverseEventsQuery(chemblId, index = 0, size = 10)
```

## Arguments

- chemblId:

  Character: ChEMBL ID of the target drug (e.g., "CHEMBL1016").

- index:

  Integer: Page index for pagination (default: 0).

- size:

  Integer: Number of records to retrieve (default: 10).

## Value

Returns a tibble containing adverse events data for the specified drug.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- adverseEventsQuery(chemblId = "CHEMBL1016", size = 10)
result <- adverseEventsQuery(chemblId = "CHEMBL1016", index = 0, size = 10)
} # }
```
