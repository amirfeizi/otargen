test_that("test pharmacogenomicsGeneQuery works", {
  skip_on_cran()
  
  result <- pharmacogenomicsGeneQuery(ensgId = "ENSG00000141510")
  
  expect_true(is.null(result) || tibble::is_tibble(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true(all(c(
      "variantRsId", "genotypeId", "variantFunctionalConsequence.id",
      "variantFunctionalConsequence.label", "haplotypeId", 
      "haplotypeFromSourceId", "isDirectTarget", "drugs", 
      "phenotypeFromSourceId", "genotypeAnnotationText", 
      "phenotypeText", "pgxCategory", "evidenceLevel", 
      "studyId", "literature"
    ) %in% colnames(result)))
  }
})
