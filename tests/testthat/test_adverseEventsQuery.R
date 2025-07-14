test_that("adverseEventsQuery returns expected structure", {
  skip_on_cran()
  result <- adverseEventsQuery(chemblId = "CHEMBL1016", size = 10)
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("name" %in% colnames(result))
})
