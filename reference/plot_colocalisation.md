# Plot GWAS colocalisation results.

Scatter plot of the H4 posterior probability (shared causal variant)
against the number of colocalising variants, with labels for each
colocalising trait and a threshold reference line at H4 = 0.8.

## Usage

``` r
plot_colocalisation(df, h4_threshold = 0.8)
```

## Arguments

- df:

  A data frame returned by
  [`gwasColocalisation`](https://amirfeizi.github.io/otargen/reference/gwasColocalisation.md).

- h4_threshold:

  Numeric: H4 threshold line to draw (default: 0.8).

## Value

A `ggplot` object.

## Examples

``` r
if (FALSE) { # \dontrun{
coloc <- gwasColocalisation(study_locus_id = "5a86bfd40d2ebecf6ce97bbe8a737512")
plot_colocalisation(coloc)
} # }
```
