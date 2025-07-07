test_that("test genomicsEnglandQuery works", {
  skip_on_cran()
  
  result <- genomicsEnglandQuery(ensemblId = "ENSG00000080815", efoId = "MONDO_0004975", size = 5)
  
  expect_true(is.null(result) || tibble::is_tibble(result))
  if (!is.null(result)) {
    expect_true(all(c("disease.id", "disease.name", "target.approvedSymbol",
                      "diseaseFromSource", "cohortPhenotypes", "confidence",
                      "allelicRequirements", "studyOverview", "studyId",
                      "literature", "approvedSymbol", "diseaseId",
                      "diseaseName", "genomicsEnglandCount") %in% colnames(result)))
  }
})