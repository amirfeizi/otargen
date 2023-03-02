test_that("qtlCredibleSet works", {
  expected <- otargen::qtlCredibleSet(studyid = "Braineac2", variantid = "1_55053079_C_T", geneid = "ENSG00000169174", biofeature = "SUBSTANTIA_NIGRA")
  expect_s3_class(expected, "data.frame")
  expect_false(is.null(dim(expected)))

})
