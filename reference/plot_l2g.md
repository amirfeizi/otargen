# Plot locus-to-gene prediction scores.

Displays candidate genes ranked by their L2G score as a horizontal bar
chart with a color gradient.

## Usage

``` r
plot_l2g(df, top_n = 15)
```

## Arguments

- df:

  A tibble returned by
  [`locus2GeneQuery`](https://amirfeizi.github.io/otargen/reference/locus2GeneQuery.md).

- top_n:

  Integer: number of top genes to display (default: 15).

## Value

A `ggplot` object.

## Examples

``` r
if (FALSE) { # \dontrun{
l2g <- locus2GeneQuery(studyLocusId = "fa375739ca2a6b825ce5cc69d117e84b")
plot_l2g(l2g)
} # }
```
