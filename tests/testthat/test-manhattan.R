test_that("test manhattan  works", {
  expected <-  otargen::manhattan(studyid = "GCST90025954" , pageindex = 0 , pagesize = 10 )
  expect_s3_class(expected, "data.frame")
  expect_false(is.null(dim(expected)))
  })
