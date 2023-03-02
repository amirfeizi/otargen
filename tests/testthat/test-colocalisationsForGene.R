test_that("test the colocalisationsForGene works", {
  expected_1 <- otargen::colocalisationsForGene(ensembl_ids = "ENSG00000169174")
  expected_2 <- otargen::colocalisationsForGene(ensembl_ids = list("ENSG00000163946", "ENSG00000169174", "ENSG00000143001"))
  expect_s3_class(expected_1, "data.frame")
  expect_s3_class(expected_2, "data.frame")
  expect_false(is.null(dim(expected_1)))
  expect_false(is.null(dim(expected_2)))
})
