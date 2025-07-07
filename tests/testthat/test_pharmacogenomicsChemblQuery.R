test_that("test pharmacogenomicsChemblQuery works", {
  skip_on_cran()
  
  result <- pharmacogenomicsChemblQuery(chemblId = "CHEMBL1016")
  
  expect_true(is.null(result) || tibble::is_tibble(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true(all(c(
      "variantRsId", "genotypeId", "variantFunctionalConsequence.id",
      "variantFunctionalConsequence.label", "target.id", 
      "target.approvedSymbol", "haplotypeId", "haplotypeFromSourceId", 
      "isDirectTarget", "phenotypeFromSourceId", "genotypeAnnotationText", 
      "phenotypeText", "pgxCategory", "evidenceLevel", "studyId", 
      "literature", "drugId"
    ) %in% colnames(result)))
  }
})