test_that("test the topOverlappedStudies", {
  expected <-  otargen::manhattan(studyid = "NEALE2_6177_1", pageindex = 1, pagesize = 50)
  expect_s3_class(expected, "data.frame")
  expect_false(is.null(dim(expected)))
})
