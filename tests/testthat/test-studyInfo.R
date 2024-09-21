test_that("test studyInfo works", {
  skip_on_cran()
  expected <- otargen::studyInfo(study_id = "GCST90002357")
  expect_s3_class(expected, "data.frame")
  expect_false(is.null(dim(expected)))
})
