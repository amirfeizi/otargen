test_that("multiplication works", {
  expected <- otargen::colocalisationsForGene(ensmbl_ids = "ENSG00000169174")
  expect_s3_class(expected, "data.frame")
  expect_error(otargen::colocalisationsForGene(ensmbl_ids = "ENSGXXXXXXXXX"))
  expect_false(is.null(dim(expected)))


})
