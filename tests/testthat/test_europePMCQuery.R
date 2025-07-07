test_that("test europePMCQuery works", {
  skip_on_cran()
  
  result <- europePMCQuery(ensemblId = "ENSG00000080815", efoId = "MONDO_0004975", size = 5)
  
  expect_true(is.null(result) || tibble::is_tibble(result))
  if (!is.null(result)) {
    expect_true(all(c("disease.name", "disease.id", "target.approvedSymbol", 
                      "target.id", "literature", "textMiningSentences", 
                      "resourceScore", "diseaseId", "europePmcCount", "cursor") %in% colnames(result)))
  }
})