test_that("test studyVariants works", {
  skip_on_cran()
  
  result <- studyVariants(study_id = "GCST90002357")
  
  expect_true(is.null(result) || is.data.frame(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true(all(c(
      "variant.id", "pval", "variant.nearestCodingGene.symbol",
      "variant.rsId", "variant.chromosome", "variant.position",
      "variant.nearestCodingGeneDistance", "credibleSetSize",
      "ldSetSize", "oddsRatio", "beta"
    ) %in% colnames(result)))
  }
})
