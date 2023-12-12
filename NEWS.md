# otargen 1.1.0

* Fixed an issue in the `variantInfo` function where the wrong variable (`result`) was being referenced, leading to an 'object not found' error. Replaced `result$variantInfo` with `var_info$variantInfo` to correctly process the API response data. Additionally, added checks to ensure that `var_info` and `var_info$variantInfo` are not `NULL` before processing, which improves the function's robustness and error handling.
* Improved the `test plot_l2g.R` test case to provide better error handling and diagnostic information. The test now uses a `tryCatch` block to gracefully handle errors and explicitly fail with a clear message, enhancing the reliability and maintainability of the test suite.
* Fixed the issue with `genesForVariant()` on giving error using rsId variants.
* Updated all functions to include connection timeout checks. Added try-catch blocks to handle timeouts during API requests gracefully, ensuring that the functions provide informative error messages and fail safely in case of network or connection issues.

# otargen 1.1.0

* This is the first release of otargen.
