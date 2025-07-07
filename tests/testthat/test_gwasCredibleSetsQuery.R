test_that("test gwasCredibleSetsQuery works", {
  skip_on_cran()
  
  result <- gwasCredibleSetsQuery(variantId = "19_10352442_G_C", size = 5, index = 0)
  
  expect_true(is.null(result) || tibble::is_tibble(result))
  if (!is.null(result)) {
    expect_true(all(c("studyLocusId", "pValueMantissa", "pValueExponent", "beta",
                      "finemappingMethod", "confidence", "variant.id",
                      "variant.chromosome", "variant.position", "variant.referenceAllele",
                      "variant.alternateAllele", "study.traitFromSource",
                      "study.id", "study.diseases", "locus.rows.posteriorProbability",
                      "locusSize.count", "l2GPredictions.rows.target.id",
                      "l2GPredictions.rows.target.approvedSymbol", "l2GPredictions.rows.score",
                      "variantId", "referenceAllele", "alternateAllele") %in% colnames(result)))
  }
})
