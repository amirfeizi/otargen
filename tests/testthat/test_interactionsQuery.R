test_that("indicationsQuery returns a tibble with expected columns", {
  skip_on_cran()  # Avoid CRAN checks failing due to network calls
  
  chembl_id <- "CHEMBL1016"
  
  result <- indicationsQuery(chemblId = chembl_id)
  
  # Check that result is either NULL or a tibble
  expect_true(is.null(result) || inherits(result, "tbl_df"))
  
  if (!is.null(result)) {
    # Check that required columns exist
    expected_cols <- c(
      "maxPhaseForIndication" , "references" 
    )
    expect_true(all(expected_cols %in% names(result)))
    
    # Check that drugId matches input
    expect_true(any(result$drugId == chembl_id))
    
    # Check that indicationsCount is numeric
    expect_true(is.numeric(result$indicationsCount))
  }
})

test_that("indicationsQuery errors if chemblId is missing", {
  expect_error(indicationsQuery(), 
               regexp = "Please provide a value for the 'chemblId' argument.")
})
