test_that("test studyLocus2GeneTable works", {
  skip_on_cran()
  expected <- otargen::studyLocus2GeneTable(study_id = "GCST003044" , variant_id = "14_88009660_C_T")
  expect_s3_class(expected, "data.frame")
  expect_false(is.null(dim(expected)))
})
