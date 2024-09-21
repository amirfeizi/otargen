test_that("test get_genes works", {
  skip_on_cran()
  expected <- otargen::getLociGenes(chromosome = "2", start = 239634984, end = 241634984)
  expect_s3_class(expected, c("tbl_df", "tbl", "data.frame"))
  expect_named(expected, c("id", "symbol","bioType","description","chromosome", "tss", "start","end","fwdStrand","exons"))
  expect_false(is.null(dim(expected)))
  expect_error(otargen::getLociGenes(chromosome = 1, start = "239634984", end = "241634984"))
})
