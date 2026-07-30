# Plot protein interaction network for a gene.

Draws a circular network layout of molecular interaction partners with
edge thickness proportional to the interaction confidence score.

## Usage

``` r
plot_interactions(df, top_n = 20)
```

## Arguments

- df:

  A tibble returned by
  [`interactionsQuery`](https://amirfeizi.github.io/otargen/reference/interactionsQuery.md).

- top_n:

  Integer: number of top interactions to display (default: 20).

## Value

A `ggplot` object.

## Examples

``` r
if (FALSE) { # \dontrun{
int <- interactionsQuery(ensgId = "ENSG00000141510",
  sourceDatabase = "intact", size = 25)
plot_interactions(int)
} # }
```
