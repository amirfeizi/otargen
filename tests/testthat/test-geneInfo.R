test_that("test geneInfo works", {
  expected_1 <- otargen::geneInfo(gene="ENSG00000004864")
  expected_2 <- otargen::geneInfo(gene="PCSK9")
  expect_s3_class(expected_1, "data.frame")
  expect_s3_class(expected_2, "data.frame")
  expect_error(otargen::geneInfo("ENSGXXXXXXXXX"))
  expect_false(is.null(dim(expected_1)))
  expect_false(is.null(dim(expected_2)))
})
