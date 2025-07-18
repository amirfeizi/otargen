# otargen 2.0.0

### Major Release

Following [Open Targets' announcement](https://community.opentargets.org/t/open-targets-genetics-will-be-deprecated-on-9-july-2025/1796), Open Targets Genetics has been officially merged into the Open Targets Platform API. Consequently, **otargen 2.0.0 represents a complete overhaul** of the package’s functionality to support this unified API endpoint and schema.

#### What's New

- 🔁 **Unified API Integration**: Full migration from the deprecated Open Targets Genetics GraphQL API to the new Platform GraphQL API.
- 🔍 **Expanded Query Support**: Includes all major data types now available in the merged schema.
- 📘 **Updated Documentation**: All help files and vignettes have been rewritten to reflect new function usage.
- 📄 **New Example Articles**: Practical use cases demonstrating the new API queries.
- ⚠️ **Deprecation Notice**: Many query functions from version 1.1.5 are now deprecated but retained for backward compatibility where possible.

We **highly recommend** upgrading to otargen 2.0.0 for the best experience with the latest Open Targets Platform API.

---

# otargen 1.1.5

### Resubmission to CRAN 
### Added frequently requested query functions from Open Target to the package
  * chemblQuery(): This function queries the Open Targets GraphQL API to retrieve ChEMBL data
for a specified gene and disease, including evidence from the ChEMBL datasource.
  * clinvarQuery(): This function queries the Open Targets  GraphQL API to retrieve ClinVar data
for a specified gene and disease, including evidence from the NCBI datasource.
  * knownDrugsQuery(): This function queries the Open Targets  GraphQL API to retrieve known drugs data
for a specified gene.
  *  mousePhenotypesQuery(): This function queries the Open Targets  GraphQL API to retrieve mouse phenotypes data
for a specified gene.
  *  compGenomicsQuery() This function queries the Open Targets  GraphQL API to retrieve comparative genomics data
for a specified gene.

# otargen 1.1.4

### Bug fixed for `overlapInfoForStudy()` function.

# otargen 1.1.3

# otargen 1.1.2

# otargen 1.1.1

### Updated the examples article
  * Fixed several inconsistencies between parameters naming in the functions used in the examples article and the
  main function in the package.
  * Streamline the examples description to be short and organized.
  * Added additional plotting case for example 7 for the additional parameters that has been added in the current version to the `plot_l2g` function. 
  
### Bug Fixes
  * Fixed and issue with the `plot_l2g()` function for the cases when the `disease` parameter were not selected. 
  The previous function were plotting only top disease for one gene. The function now updated with two more parameters to expand the `plot_l2g()` functionality for plotting disease agnostic scenario. These two parameters are:
  - `top_n_disease` (default: `1`): Determines the number of top diseases to plot for each gene, ranked by L2G score. Increase this value to include more diseases in the analysis. Use in conjunction with being disease-agnostic to dynamically select the top diseases for each gene.

- `l2g_cutoff` (default: `0.5`): Sets the minimum L2G score threshold for diseases to be considered in the plot. Increasing this value can help reduce plot clutter by focusing on higher-confidence associations.

# otargen 1.1.0

### Bug Fixes
* Fixed an issue in the `variantInfo` function where the wrong variable (`result`) was being referenced, leading to an 'object not found' error. Modified `variantInfo()` to correctly process the API response data. Additionally, added checks to ensure that the results are not `NULL` before processing, which improves the function's robustness and error handling.

### Test Suite Enhancement
* Improved the `test plot_l2g.R` test case to provide better error handling and diagnostic information. The test now uses a `tryCatch` block to gracefully handle errors and explicitly fail with a clear message, enhancing the reliability and maintainability of the test suite.

### Bug Fix
* Fixed the issue with `genesForVariant()` on giving an error when using `rsId` variants.

### Code Improvement
* Updated all functions to include connection timeout checks. Added try-catch blocks to handle timeouts during API requests gracefully, ensuring that the functions provide informative error messages and fail safely in case of network or connection issues.

# otargen 1.0.0

### Initial Release
* This is the first release of otargen.
