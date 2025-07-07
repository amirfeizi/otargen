test_that("test sharedTraitStudiesQuery works", {
  skip_on_cran()
  
  result <- sharedTraitStudiesQuery(
    diseaseIds = c("EFO_0004587"),
    size = 2,
    index = 0
  )
  
  expect_true(is.null(result) || tibble::is_tibble(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true(all(c(
      "id", "traitFromSource", "projectId",
      "publicationFirstAuthor", "publicationDate",
      "publicationJournal", "nSamples", "cohorts",
      "ldPopulationStructure", "pubmedId"
    ) %in% colnames(result)))
  }
})