test_that("test phewas works", {
  expected_1 <- otargen::pheWAS(variantid = "1_55063514_G_A")
  expected_2 <- otargen::pheWAS(variantid = "rs72698179")
  expect_s3_class(expected_1, "data.frame")
  expect_s3_class(expected_2, "data.frame")
  expect_false(is.null(dim(expected_1)))
  expect_false(is.null(dim(expected_2)))
})
