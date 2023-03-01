test_that("test studiesAndLeadVariantsForGene works", {
  expected <- otargen::studiesAndLeadVariantsForGene(ensembl_ids = "ENSG00000169174")
  expect_s3_class(expected, "data.frame")
  expect_false(is.null(dim(expected)))
  })
