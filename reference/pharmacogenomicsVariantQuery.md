# Retrieve Pharmacogenomics data for a specified variant.

This function queries the Open Targets GraphQL API to retrieve
pharmacogenomics data for a specified variant.

## Usage

``` r
pharmacogenomicsVariantQuery(variantId)
```

## Arguments

- variantId:

  Character: ID of the target variant (e.g., "12_111446804_T_C").

## Value

Returns a tibble containing pharmacogenomics data for the specified
variant.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- pharmacogenomicsVariantQuery(variantId = "12_111446804_T_C")
} # }
```
