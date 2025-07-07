test_that("test gwasRegional works", {
  skip_on_cran()
  
  result <- gwasRegional(
    study_id = "GCST90002357",
    chromosome = "1",
    start = 153992685,
    end = 154155116
  )
  
  expect_true(is.null(result) || tibble::is_tibble(result) || is.data.frame(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true(all(c("variant.id", "variant.chromosome", "variant.position", "pval") %in% colnames(result)))
  }
})
