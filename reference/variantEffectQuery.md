# Retrieve Variant Effect data for a specified variant.

This function queries the Open Targets GraphQL API to retrieve variant
effect data for a specified variant.

## Usage

``` r
variantEffectQuery(variantId)
```

## Arguments

- variantId:

  Character: ID of the target variant (e.g., "4_1804392_G_A").

## Value

Returns a tibble containing variant effect data for the specified
variant.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- variantEffectQuery(variantId = "4_1804392_G_A")
} # }
```
