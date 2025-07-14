test_that("test knownDrugsGeneQuery works", {
  skip_on_cran()
  
  result <- knownDrugsGeneQuery(
    ensgId = "ENSG00000169174",
    size = 5
  )
  
  expect_true(is.null(result) || tibble::is_tibble(result) || is.data.frame(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true(all(c(
      "phase", "status", "drug.id", "drug.name"
    ) %in% names(result)))
  }
})
