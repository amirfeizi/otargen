# Retrieve Pharmacogenomics data for a specified drug.

This function queries the Open Targets GraphQL API to retrieve
pharmacogenomics data for a specified drug.

## Usage

``` r
pharmacogenomicsChemblQuery(chemblId)
```

## Arguments

- chemblId:

  Character: ChEMBL ID of the target drug (e.g., "CHEMBL1016").

## Value

Returns a tibble containing pharmacogenomics data for the specified
drug.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- pharmacogenomicsChemblQuery(chemblId = "CHEMBL1016")
} # }
```
