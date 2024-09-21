test_that("test gwasCredibleSet works", {
  skip_on_cran()
  expected <- otargen::gwasCredibleSet(study_id = "GCST006614_3" ,variant_id = "1_55058182_G_A")
  expect_s3_class(expected, "data.frame")
  expect_false(is.null(dim(expected)))
})
