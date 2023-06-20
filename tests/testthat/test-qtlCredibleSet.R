test_that("test qtlCredibleSet works", {
  expected_1 <- otargen::qtlCredibleSet(study_id = "Braineac2", variant_id = "1_55053079_C_T", gene = "ENSG00000169174", biofeature = "SUBSTANTIA_NIGRA")
  expected_2 <- otargen::qtlCredibleSet(study_id="Braineac2", variant_id="rs7552841", gene="PCSK9", biofeature="SUBSTANTIA_NIGRA")
  expect_s3_class(expected_1, "data.frame")
  expect_s3_class(expected_2, "data.frame")
  expect_false(is.null(dim(expected_1)))
  expect_false(is.null(dim(expected_2)))

})
