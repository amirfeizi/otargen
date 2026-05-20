# Retrieve Variants data for a specified study locus.

This function queries the Open Targets GraphQL API to retrieve variants
data for a specified study locus.

## Usage

``` r
variantsQuery(studyLocusId, size = 500, index = 0)
```

## Arguments

- studyLocusId:

  Character: ID of the target study locus (e.g.,
  "fa375739ca2a6b825ce5cc69d117e84b").

- size:

  Integer: Number of records to retrieve (default: 500).

- index:

  Integer: Page index for pagination (default: 0).

## Value

Returns a tibble containing variants data for the specified study locus.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- variantsQuery(studyLocusId = "fa375739ca2a6b825ce5cc69d117e84b", 
size = 500, index = 0)
} # }
```
