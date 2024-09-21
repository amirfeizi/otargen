test_that("test indexVariantsAndStudiesForTagVariant works", {
  skip_on_cran()
  expected <- otargen::indexVariantsAndStudiesForTagVariant(variant_id = "1_109274968_G_T", pageindex = 1, pagesize = 50)
  expect_s3_class(expected, "data.frame")
  expect_false(is.null(dim(expected)))
})
