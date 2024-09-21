test_that("test the topOverlappedStudies  works", {
  skip_on_cran()
  expected <-  otargen::manhattan(study_id = "NEALE2_6177_1", pageindex = 1, pagesize = 50)
  expect_s3_class(expected, "data.frame")
  expect_false(is.null(dim(expected)))
})
