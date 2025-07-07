test_that("chemicalProbesQuery returns probe information", {
  skip_on_cran()
  result <- chemicalProbesQuery(geneSymbol = "EGFR")
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("probeName" %in% colnames(result))
})
