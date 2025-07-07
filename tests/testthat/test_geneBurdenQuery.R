test_that("test geneBurdenQuery works", {
  skip_on_cran()
  
  result <- geneBurdenQuery(ensemblId = "ENSG00000137642", efoId = "MONDO_0004975", size = 5)
  
  expect_true(is.null(result) || tibble::is_tibble(result))
  if (!is.null(result)) {
    expect_true(all(c("disease.id", "disease.name", "diseaseFromSource", 
                      "target.id", "target.approvedSymbol", "releaseVersion", 
                      "targetFromSourceId", "urls.url", "variantEffect", 
                      "directionOnTrait", "allelicRequirements", "studyId", 
                      "ancestry", "ancestryId", "resourceScore", "cohortId", 
                      "projectId", "statisticalMethod", "statisticalMethodOverview", 
                      "studyCases", "studyCasesWithQualifyingVariants", 
                      "studySampleSize", "oddsRatio", "oddsRatioConfidenceIntervalLower", 
                      "oddsRatioConfidenceIntervalUpper", "beta", 
                      "betaConfidenceIntervalLower", "betaConfidenceIntervalUpper", 
                      "pValueMantissa", "pValueExponent", "literature", 
                      "diseaseId", "geneBurdenCount") %in% colnames(result)))
  }
})