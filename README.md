
# otargen <img src="man/figures/logo.jpg" align="right" width="120" />


<!-- badges: start -->
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/otargen?color=blue)]( https://CRAN.R-project.org/package=otargen)
[![Downloads](https://cranlogs.r-pkg.org/badges/otargen?color=yellow)](https://CRAN.R-project.org/package=otargen) 
<!-- badges: end -->

## Simplify Your Data Retrieval and Analysis from Open Target Genetics ✨
`otargen` is an innovative and open-source R package designed to streamline data retrieval
and analysis from the renowned [Open Target Genetics](https://genetics.opentargets.org/) portal.
With `otargen`, harnessing the power of Open Target Genetics becomes effortless within the familiar R programming environment. 

### Effortless Data Retrieval 🚀

Retrieve data effortlessly with a comprehensive suite of functions that cover all
[GraphQL](https://api.genetics.opentargets.org/graphql/schema) query types in the Open Target Genetics schema. 
Say goodbye to complex queries with nested output and hello to clean, tidy data tables ready for analysis.

### Insightful Visualizations 📈

Use several intuitive plotting functions for multiple important and complex outputs from `otargen`'s
data retrieval functions. This allows to visualize and explore these complex data tables effortlessly,
gaining valuable insights and uncovering patterns that matter.

### Detailed Documentation 📖

Each function in `otargen` comes with detailed documentation, providing insights into the underlying GraphQL query
and the structure of the returned data. You'll have a clear understanding of how to use each function and how to
interpret the results, making integration with your analysis seamless and straightforward.

### Continuous Development 👨‍💻

Open Target Genetics is continuously upgrading its data and functionalities, and `otargen` is committed to keeping up
with these changes. We strive to provide regular updates and improvements to ensure that you always have access to
the latest features and data.

We welcome contributions from the community to enhance and expand `otargen`. Whether it's submitting bug reports,
suggesting new features, or contributing code, your contributions are valuable and appreciated.
Please check `otargen` on [GitHub](https://github.com/amirfeizi/otargen).

### Installation

Install `otargen` from CRAN:

```r
install.packages("otargen")
```



Or install the latest development version from GitHub (requires devtools package):

```r
if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("https://github.com/amirfeizi/otargen.git")
```

## Citing this work

`otargen` was published on 19th July 2023 in Bioinformatics:
<https://doi.org/10.1093/bioinformatics/btad441>.

To generate a citation for this publication from within R:

``` r
citation("otargen")
To cite package ‘otargen’ in publications use:

  Feizi A, Ray K (2023). _otargen: Access Open Target Genetics_. R package version 1.0.0,
  <https://CRAN.R-project.org/package=otargen>.

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {otargen: Access Open Target Genetics},
    author = {Amir Feizi and Kamalika Ray},
    year = {2023},
    note = {R package version 1.0.0},
    url = {https://CRAN.R-project.org/package=otargen},}
```
