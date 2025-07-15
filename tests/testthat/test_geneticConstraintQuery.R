test_that("test geneticConstraintQuery works", {
  skip_on_cran()
  
  result <- geneticConstraintQuery(ensgId = "ENSG00000141510")
  
  expect_true(is.null(result) || tibble::is_tibble(result))
  if (!is.null(result)) {
    expect_true(all(c( "constraintType","score",  "upperBin", "upperBin6",  "geneId",   "approvedSymbol") %in% names(result)))
  }
})
