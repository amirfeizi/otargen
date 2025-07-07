test_that("test pharmacogenomicsVariantQuery works", {
  skip_on_cran()
  
  result <- pharmacogenomicsVariantQuery(variantId = "12_111446804_T_C")
  
  expect_true(is.null(result) || tibble::is_tibble(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true(all(c(
      "genotypeId", "isDirectTarget", "target.id", 
      "target.approvedSymbol", "drugs", "phenotypeFromSourceId", 
      "genotypeAnnotationText", "phenotypeText", "pgxCategory", 
      "evidenceLevel", "studyId", "literature",
      "variantId", "referenceAllele", "alternateAllele"
    ) %in% colnames(result)))
  }
})