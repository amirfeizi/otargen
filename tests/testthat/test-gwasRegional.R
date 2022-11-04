test_that("test gwasRegional works", {
  expected <- otargen::gwasRegional(studyid = "GCST90025954" , chromosome = "1" , start = 55058000 , end = 55059000 )
  expect_s3_class(expected, "data.frame")
  expect_false(is.null(expected))
  })
