test_that("test tagVariantsAndStudiesForIndexVariant works", {
  expected <- otargen::tagVariantsAndStudiesForIndexVariant(variant_id = "1_55063514_G_A" , pageindex = 0 , pagesize = 10 )
  expect_s3_class(expected, "data.frame")
  expect_false(is.null(dim(expected)))
})
