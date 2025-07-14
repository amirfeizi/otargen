test_that("test geneBurdenQuery works", {
  skip_on_cran()
  
  result <- geneBurdenQuery(ensemblId = "ENSG00000137642", efoId = "MONDO_0004975", size = 5)
  
  expect_true(is.null(result) || tibble::is_tibble(result))
  if (!is.null(result)) {
    expect_true(all(c("disease.id", "disease.name") %in% colnames(result)))
  }
})