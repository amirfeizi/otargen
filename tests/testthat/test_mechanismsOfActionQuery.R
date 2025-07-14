test_that("test mechanismsOfActionQuery works", {
  skip_on_cran()
  
  result <- mechanismsOfActionQuery(chemblId = "CHEMBL1016")
  
  expect_true(is.null(result) || tibble::is_tibble(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true(all(c(
      "mechanismOfAction", "targetName", "targets"
    ) %in% names(result)))
  }
})
