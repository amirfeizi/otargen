test_that("test studyLocus2GeneTable works", {
  expected <- otargen::studyLocus2GeneTable(studyid = "GCST003044" , variantid = "14_88009660_C_T")
  expect_s3_class(expected, "data.frame")
  expect_false(is.null(dim(expected)))
})
