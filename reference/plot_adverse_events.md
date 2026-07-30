# Plot adverse events for a drug as a lollipop chart.

Displays the top adverse events ranked by log-likelihood ratio (logLR)
with a dashed reference line at the critical value threshold.

## Usage

``` r
plot_adverse_events(df, top_n = 20)
```

## Arguments

- df:

  A tibble returned by
  [`adverseEventsQuery`](https://amirfeizi.github.io/otargen/reference/adverseEventsQuery.md).

- top_n:

  Integer: number of top events to display (default: 20).

## Value

A `ggplot` object.

## Examples

``` r
if (FALSE) { # \dontrun{
ae <- adverseEventsQuery(chemblId = "CHEMBL941")
plot_adverse_events(ae)
} # }
```
