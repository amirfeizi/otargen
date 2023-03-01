test_that("test studiesAndLeadVariantsForGeneByL2G", {
  expected <- otargen::studiesAndLeadVariantsForGeneByL2G(ensmbl_ids = "ENSG00000169174")
  expect_s3_class(expected, "data.frame")
  expect_error(otargen::studiesAndLeadVariantsForGeneByL2G(ensmbl_ids = "ENSGXXXXXXXXX"))
  expect_false(is.null(dim(expected)))
})
