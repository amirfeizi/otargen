# <img src="man/figures/logo.jpg" align="right" width="120" />

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/otargen?color=blue)](https://CRAN.R-project.org/package=otargen)
[![Downloads](https://cranlogs.r-pkg.org/badges/otargen?color=yellow)](https://CRAN.R-project.org/package=otargen)
[![License](https://img.shields.io/badge/License-MIT-blue.svg?color=green)](https://opensource.org/licenses/MIT)
[![Build Status](https://app.travis-ci.com/your-username/your-repository.svg?branch=master)](https://app.travis-ci.com/your-username/your-repository)



## Streamlined Tidy Data Access to Open Target Genetics GraphQL API🔍
`otargen` is an open-source R package for easy data retrieval and analysis from [Open Target Genetics](https://genetics.opentargets.org). It brings simplicity in scale to analsye human genetic evidances for gene-trait/diseases associations from R environment, enhancing your research with advanced data handling and visualization tools.

---
:loudspeaker: **otargen 1.1.5**

In this new release, several frequently requested query functions from Open Target are added to the package to expand the package functionality in accessing data that are key in evaluating drug targets. In addition, multiple technical bugs has been fixed to increase the efficiency of the previous  query functions data retrieval. The added new query functions include:

-  ` chemblQuery()`: This function queries the Open Targets GraphQL API to retrieve ChEMBL data
for a specified gene and disease, including evidence from the ChEMBL datasource. 

▶️ _These data are key in evaluating the tractability of a target._
  
-  `clinvarQuery()`:  This function queries the Open Targets  GraphQL API to retrieve ClinVar data
for a specified gene and disease, including evidence from the NCBI datasource. 

▶️ _These data are key in evaluating the target MoA and disease biology._
  
- `knownDrugsQuery()`: This function queries the Open Targets  GraphQL API to retrieve known drugs data
for a specified gene. 

▶️ _These data are key to evaluate the competitive landscape of a target._
 
- `mousePhenotypesQuery()`: This function queries the Open Targets  GraphQL API to retrieve mouse phenotypes data
for a specified gene. 

▶️ _These data are key in evaluating the target validation._
 
-  `compGenomicsQuery()`: This function queries the Open Targets  GraphQL API to retrieve comparative genomics data
for a specified gene. 

▶️ _These data are key in evaluating the safety of a target._ 

**Citing otargen in important for us** :raising_hand_man:
Please cite `otargen` if you use it in your research 🙏🏻 : . [Bioinformatics](https://doi.org/10.1093/bioinformatics/btad441).

---



:loudspeaker: `otargenpy`  is available for python users :snake: `pip install otargenpy`

Check out the repository here: [otargenpy on GitHub](https://github.com/amirfeizi/otargenpy). `otargenpy` has all the capabilities of `otargen`, We encourage you to try it out and provide us with feedback!

### Key Features
🚀 **Effortless Data Retrieval**: Easily access and transform complex GraphQL queries.

📈 **Insightful Visualizations**: Clearly visualize complex datasets with intuitive plots.

📖 **Comprehensive Documentation**: Well-documented functions for easy integration.

👨‍💻 **Regular Updates**: Stay updated with new features and improvements.



### Installation
Get started with `otargen` from CRAN:

```r
install.packages("otargen")
```
Or install the latest development version from GitHub:
```
if (!require("devtools")) install.packages("devtools")
devtools::install_github("https://github.com/amirfeizi/otargen.git")
```

### Community and Contributions
Join the `otargen` community on [GitHub](https://github.com/amirfeizi/otargen) and contribute to its growth. We welcome bug reports, feature suggestions, and code contributions.


