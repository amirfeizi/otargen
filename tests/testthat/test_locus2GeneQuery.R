test_that("test locus2GeneQuery works", {
  skip_on_cran()
  
  result <- locus2GeneQuery(studyLocusId = "fa375739ca2a6b825ce5cc69d117e84b")
  
  expect_true(is.null(result) || tibble::is_tibble(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true(all(c(
      "shapBaseValue", "features", "score",
      "target.id", "target.approvedSymbol", "studyLocusId"
    ) %in% colnames(result)))
  }
})