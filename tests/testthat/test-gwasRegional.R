test_that("test gwasRegional works", {
  skip_on_cran()
  expected <- otargen::gwasRegional(study_id = "GCST90025954" , chromosome = "1" , start = 55058000 , end = 55059000 )
  expect_s3_class(expected, "data.frame")
  expect_false(is.null(expected))
  })
