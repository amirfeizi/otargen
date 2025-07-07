test_that("test genesForVariant works", {
  skip_on_cran()
  
  result <- genesForVariant(variant_id = "rs4129267")
  
  expect_type(result, "list")
  expect_true(all(c("v2g", "tssd", "qtls", "chromatin", "functionalpred") %in% names(result)))
  
  if (!is.null(result$v2g)) {
    expect_true(is.data.frame(result$v2g))
    expect_true(all(c("gene.symbol", "variant", "overallScore", "gene.id") %in% colnames(result$v2g)))
  }
})