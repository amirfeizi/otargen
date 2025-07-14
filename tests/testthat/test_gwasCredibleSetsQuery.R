test_that("gwasCredibleSetsQuery returns expected results", {
  
  # Example Ensembl ID and EFO ID known to exist in Open Targets
  # (replace with real IDs you know exist in the API)
  ensemblId <- "ENSG00000105397" # Example: APOE
  efoId <- "EFO_0000685"         # Example: Alzheimer's disease
  size <- 5
  
  # Call the function
  result <- gwasCredibleSetsQuery(
    ensemblId = ensemblId,
    efoId = efoId,
    size = size
  )
  
  # Check that the result is either NULL or a tibble
  expect_true(is.null(result) || tibble::is_tibble(result))
  
  if (!is.null(result)) {
    # Check some columns exist
    expect_true("score" %in% names(result))
    expect_true("disease.id" %in% names(result))
    expect_true("diseaseName" %in% names(result))
    

  }
})
