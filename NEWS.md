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
