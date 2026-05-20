# Retrieve QTL Credible Sets data for a specified variant.

This function queries the Open Targets GraphQL API to retrieve QTL
credible sets data for a specified variant.

## Usage

``` r
qtlCredibleSetsQuery(variantId, size = 500, index = 0)
```

## Arguments

- variantId:

  Character: ID of the target variant (e.g., "19_10352442_G_C").

- size:

  Integer: Number of records to retrieve (default: 500).

- index:

  Integer: Page index for pagination (default: 0).

## Value

Returns a tibble containing QTL credible sets data for the specified
variant.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- qtlCredibleSetsQuery(variantId = "19_10352442_G_C", size = 500, 
index = 0)
} # }
```
