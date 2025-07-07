test_that("adverseEventsQuery returns expected structure", {
  skip_on_cran()
  result <- adverseEventsQuery(drug = "aspirin")
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("adverseEvent" %in% colnames(result))
})
