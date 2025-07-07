test_that("chemblQuery returns chemical bioactivity data", {
  skip_on_cran()
  result <- chemblQuery(chemblId = "CHEMBL25")
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("targetName" %in% colnames(result))
})
