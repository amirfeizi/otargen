# otargen <img src="man/figures/logo.jpg" align="right" width="120" />

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/otargen?color=blue)](https://CRAN.R-project.org/package=otargen)
[![Downloads](https://cranlogs.r-pkg.org/badges/otargen?color=yellow)](https://CRAN.R-project.org/package=otargen)

## Streamline Data Analysis with Open Target Genetics 🔍
`otargen` is an innovative, open-source R package designed for easy data retrieval and analysis from [Open Target Genetics](https://genetics.opentargets.org). It brings simplicity and power to your R environment, enhancing your research with advanced data handling and visualization tools.

### Key Features
- **Effortless Data Retrieval**: Access comprehensive GraphQL query types with user-friendly functions, transforming complex queries into tidy, analysis-ready data tables.
- **Insightful Visualizations**: Deploy intuitive plotting functions for a clear view of complex datasets, uncovering hidden patterns and insights.
- **Comprehensive Documentation**: Each function is thoroughly documented, ensuring a smooth integration into your workflow and a clear understanding of the results.
- **Regular Updates**: Stay abreast of the latest in Open Target Genetics with our commitment to continuous improvement and feature expansion.

## Announcement :loudspeaker:

For Python users, we're excited to announce that a Python version of our package is now available! :snake: :rocket: 

Check out the repository here: [otargenpy on GitHub](https://github.com/amirfeizi/otargenpy)

This version brings the capabilities of `otargen` to the Python community, We encourage you to try it out and provide feedback!

### Community and Contributions
Join the `otargen` community on [GitHub](https://github.com/amirfeizi/otargen) and contribute to its growth. We welcome bug reports, feature suggestions, and code contributions.

### Installation
Get started with `otargen` from CRAN:
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
