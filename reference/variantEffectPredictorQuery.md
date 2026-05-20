# Retrieve Variant Effect Predictor data for a specified variant.

This function queries the Open Targets GraphQL API to retrieve variant
effect predictor data for a specified variant.

## Usage

``` r
variantEffectPredictorQuery(variantId)
```

## Arguments

- variantId:

  Character: ID of the target variant (e.g., "4_1804392_G_A").

## Value

Returns a tibble containing variant effect predictor data for the
specified variant.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- variantEffectPredictorQuery(variantId = "4_1804392_G_A")
} # }
```
