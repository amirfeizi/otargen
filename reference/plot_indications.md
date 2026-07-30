# Plot drug indications by clinical stage.

Faceted horizontal bar chart showing disease indications grouped by
clinical trial stage. Each stage gets its own panel listing the
individual disease names.

## Usage

``` r
plot_indications(df, top_n = 10)
```

## Arguments

- df:

  A tibble returned by
  [`indicationsQuery`](https://amirfeizi.github.io/otargen/reference/indicationsQuery.md).

- top_n:

  Integer: max diseases to show per stage panel (default: 10).

## Value

A `ggplot` object.

## Examples

``` r
if (FALSE) { # \dontrun{
ind <- indicationsQuery(chemblId = "CHEMBL941")
plot_indications(ind)
} # }
```
