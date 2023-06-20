test_that("test studyInfo works", {
  expected <- otargen::studyInfo(study_id = "GCST90002357")
  expect_s3_class(expected, "data.frame")
  expect_false(is.null(dim(expected)))
})
