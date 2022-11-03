test_that("test overlapinfoForStudy works", {
  expected <- otargen::overlapInfoForStudy(studyid = "GCST90025954" , studyids = c("GCST006612","GCST010245" ,"GCST90038690"))
  expect_s3_class(expected, "data.frame")
  expect_false(is.null(dim(expected)))
})
