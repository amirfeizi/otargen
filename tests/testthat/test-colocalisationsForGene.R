test_that("test the colocalisationsForGene works", {
  expected <- otargen::colocalisationsForGene(ensembl_ids = "ENSG00000169174")
  expect_s3_class(expected, "data.frame")
  expect_error(otargen::colocalisationsForGene(ensembl_ids = "ENSGXXXXXXXXX"))
  expect_false(is.null(dim(expected)))
})
