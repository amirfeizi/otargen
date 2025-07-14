test_that("test variantEffectPredictorQuery works", {
  skip_on_cran()
  
  result <- variantEffectPredictorQuery(variantId = "4_1804392_G_A")
  
  expect_true(is.null(result) || tibble::is_tibble(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true(all(c(
      "variantConsequences","aminoAcidChange","uniprotAccessions", "codons"  
    ) %in% colnames(result)))
  }
})
