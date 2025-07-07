test_that("test variantsQuery works", {
  skip_on_cran()
  
  result <- variantsQuery(
    studyLocusId = "fa375739ca2a6b825ce5cc69d117e84b",
    size = 2,
    index = 0
  )
  
  expect_true(is.null(result) || tibble::is_tibble(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true(all(c(
      "logBF", "posteriorProbability", "variant.id",
      "variant.chromosome", "variant.position", "pValueMantissa"
    ) %in% colnames(result)))
  }
})