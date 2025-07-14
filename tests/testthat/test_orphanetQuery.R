test_that("test orphanetQuery works", {
  skip_on_cran()
  
  result <- orphanetQuery(
    ensemblId = "ENSG00000080815",
    efoId = "MONDO_0004975",
    size = 5
  )
  
  expect_true(is.null(result) || tibble::is_tibble(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true(all(c(
      "variantEffect", "directionOnTrait", "diseaseFromSource",
      "diseaseFromSourceId"
    ) %in% names(result)))
  }
})
