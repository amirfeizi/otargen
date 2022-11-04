test_that("qtlCredibleSet works", {
  expected <- otargen::qtlCredibleSet(studyId = "Braineac2", variantId = "1_55053079_C_T", geneId = "ENSG00000169174", bioFeature = "SUBSTANTIA_NIGRA")
  expect_s3_class(expected, "data.frame")
  expect_false(is.null(dim(expected)))

})
