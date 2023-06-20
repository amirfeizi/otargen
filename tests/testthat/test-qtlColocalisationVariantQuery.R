test_that("test qtlColocalisationVariantQuery works", {
  expected <- otargen::qtlColocalisationVariantQuery(study_id = "GCST90025954" ,variant_id = "1_55029009_C_T")
  expect_s3_class(expected, "data.frame")
  expect_false(is.null(dim(expected)))
})
