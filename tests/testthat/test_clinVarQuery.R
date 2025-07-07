test_that("clinVarQuery returns clinical variant data", {
  skip_on_cran()
  result <- clinVarQuery(geneSymbol = "BRCA1")
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("clinicalSignificance" %in% colnames(result))
})
