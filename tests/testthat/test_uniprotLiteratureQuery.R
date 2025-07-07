test_that("test uniprotLiteratureQuery works", {
  skip_on_cran()
  
  result <- uniprotLiteratureQuery(
    ensemblId = "ENSG00000130203",
    efoId = "MONDO_0004975",
    size = 2
  )
  
  expect_true(is.null(result) || tibble::is_tibble(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true(all(c(
      "disease.id", "disease.name", "diseaseFromSource",
      "targetFromSourceId", "studyId", "literature", "confidence"
    ) %in% colnames(result)))
  }
})