test_that("test depMapQuery works", {
  skip_on_cran()
  
  result <- depMapQuery(ensgId = "ENSG00000141510")
  
  expect_true(is.null(result) || tibble::is_tibble(result))
  if (!is.null(result)) {
    expect_true(all(c("tissueName", "screens.depmapId", "screens.cellLineName", 
                      "screens.diseaseFromSource", "screens.geneEffect", 
                      "screens.expression") %in% colnames(result)))
  }
})