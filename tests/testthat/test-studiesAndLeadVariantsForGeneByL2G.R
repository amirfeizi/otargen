test_that("test studiesAndLeadVariantsForGeneByL2G works", {
  expected_1 <- otargen::studiesAndLeadVariantsForGeneByL2G(genes="ENSG00000169174")
  expected_2 <- otargen::studiesAndLeadVariantsForGeneByL2G(genes=list("PCSK9", "TASOR"))
  expect_s3_class(expected_1, "data.frame")
  expect_s3_class(expected_2, "data.frame")
  expect_error(otargen::studiesAndLeadVariantsForGeneByL2G(genes = "ENSGXXXXXXXXX"))
  expect_false(is.null(dim(expected_1)))
  expect_false(is.null(dim(expected_2)))
})
