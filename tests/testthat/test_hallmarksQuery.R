test_that("test hallmarksQuery works", {
  skip_on_cran()
  
  result <- hallmarksQuery(ensgId = "ENSG00000141510")
  
  expect_true(is.null(result) || tibble::is_tibble(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true(all(c("name", "pmid", "description", "type", "geneId") %in% colnames(result)) ||
                  all(c("pmid", "impact", "description", "label", "type", "geneId") %in% colnames(result)))
  }
})