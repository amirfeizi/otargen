test_that("chemblQuery returns chemical bioactivity data", {
  skip_on_cran()
  result <- chemblQuery(ensemblId = "ENSG00000080815", efoId = 
                           "MONDO_0004975",
                           size = 10)
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("drug.name" %in% colnames(result))
})
