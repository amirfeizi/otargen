test_that("test pathwaysQuery works", {
  skip_on_cran()
  
  result <- pathwaysQuery(ensgId = "ENSG00000105397")
  
  expect_true(is.null(result) || tibble::is_tibble(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true(all(c("pathwayId", "pathway", "topLevelTerm") %in% colnames(result)))
  }
})