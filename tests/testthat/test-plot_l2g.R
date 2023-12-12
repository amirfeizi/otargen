test_that("test plot_l2g works", {
  # Try-Catch Block to Handle Errors Gracefully
  tryCatch({
    # Perform the operations and store the results
    result_1 <- otargen::studiesAndLeadVariantsForGeneByL2G(c("ENSG00000167207","ENSG00000096968")) %>%
      otargen::plot_l2g()
    result_2 <- otargen::studiesAndLeadVariantsForGeneByL2G(c("ENSG00000167207","ENSG00000096968")) %>%
      otargen::plot_l2g(disease = "EFO_0003767")

    # Check that the results are of the expected class
    expect_s3_class(result_1, "ggplot")
    expect_s3_class(result_2, "ggplot")

    # Check that the results are not null
    expect_false(is.null(result_1))
    expect_false(is.null(result_2))
  }, error = function(e) {
    # On error, print the error message for diagnosis
    message("Error in test plot_l2g: ", e$message)
    # You can add additional logging here if necessary

    # Fail the test explicitly
    expect_true(FALSE, "An error occurred in plotting.")
  })
})
