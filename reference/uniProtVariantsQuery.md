# Retrieve UniProt Variants data for a specified variant.

This function queries the Open Targets GraphQL API to retrieve UniProt
variants data for a specified variant.

## Usage

``` r
uniProtVariantsQuery(variantId)
```

## Arguments

- variantId:

  Character: ID of the target variant (e.g., "4_1804392_G_A").

## Value

Returns a tibble containing UniProt variants data for the specified
variant.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- uniProtVariantsQuery(variantId = "4_1804392_G_A")
} # }
```
