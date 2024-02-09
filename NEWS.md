# otargen 1.1.1

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
