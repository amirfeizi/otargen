test_that("test phewas works", {
  expected <- otargen::pheWAS(variantid = "1_55063514_G_A")
  expect_s3_class(expected, "data.frame")
  expect_false(is.null(dim(expected)))
})
