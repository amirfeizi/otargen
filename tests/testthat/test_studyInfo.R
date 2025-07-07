test_that("test studyInfo works", {
  skip_on_cran()
  
  result <- studyInfo(study_id = "GCST90002357")
  
  expect_true(is.null(result) || tibble::is_tibble(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true(all(c(
      "studyId", "traitReported", "source", "traitEfos", "pmid",
      "pubDate", "pubJournal", "pubTitle", "pubAuthor", "hasSumstats",
      "ancestryInitial", "nInitial", "nReplication", "traitCategory",
      "numAssocLoci", "nTotal"
    ) %in% colnames(result)))
  }
})