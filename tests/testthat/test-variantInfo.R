test_that("test variantInfo works", {
  expected_1 <- otargen::variantInfo(variantid = "1_55039974_G_T")
  expected_2 <- otargen::variantInfo(variantid = "rs11591147")
  expect_s3_class(expected_1, "data.frame")
  expect_s3_class(expected_2, "data.frame")
  expect_false(is.null(dim(expected_1)))
  expect_false(is.null(dim(expected_2)))
  expect_error(otargen::variantInfo())
})
