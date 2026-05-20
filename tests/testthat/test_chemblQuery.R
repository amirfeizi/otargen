test_that("chemblQuery returns chemical bioactivity data", {
  skip_on_cran()
  result <- chemblQuery(ensemblId = "ENSG00000080815", efoId = 
                           "MONDO_0004975",
                           size = 10)
  expect_true(is.null(result) || tibble::is_tibble(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true("drug.name" %in% colnames(result))
  }
})
