test_that("test geneInfo works", {
  expected <- otargen::geneInfo(geneid = "ENSG00000004864")
  expect_s3_class(expected, "data.frame")
  expect_error(otargen::geneInfo("ENSGXXXXXXXXX"))
  expect_false(is.null(dim(expected)))
})
