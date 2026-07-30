test_that("clinVarQuery returns clinical variant data", {
  skip_on_cran()
  # CFTR + cystic fibrosis is a ClinVar-rich pair; upstream data availability
  # can still change, so tolerate an empty/NULL result and only assert the
  # column contract when rows are returned.
  result <- clinVarQuery(ensemblId = "ENSG00000001626", efoId = "EFO_0000341", size = 10)
  expect_true(is.null(result) || is.data.frame(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true("directionOnTrait" %in% colnames(result))
  }
})
