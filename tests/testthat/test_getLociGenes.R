test_that("test getLociGenes works", {
  skip_on_cran()
  
  result <- getLociGenes(chromosome = "2", start = 239634984, end = 241634984)
  
  expect_true(is.null(result) || tibble::is_tibble(result) || is.data.frame(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true(all(c("id", "symbol", "bioType", "description", "chromosome",
                      "tss", "start", "end", "fwdStrand", "exons") %in% colnames(result)))
  }
})