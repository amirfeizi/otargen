# Retrieve Locus-to-Gene Predictions data for a specified study locus.

This function queries the Open Targets GraphQL API to retrieve
locus-to-gene prediction data for a specified study locus.

## Usage

``` r
locus2GeneQuery(studyLocusId)
```

## Arguments

- studyLocusId:

  Character: ID of the target study locus (e.g.,
  "fa375739ca2a6b825ce5cc69d117e84b").

## Value

Returns a tibble containing locus-to-gene prediction data for the
specified study locus.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- locus2GeneQuery(studyLocusId = "fa375739ca2a6b825ce5cc69d117e84b")
} # }
```
