test_that("test plot_phewas works", {
  skip_on_cran()
  expected_1 <- otargen::pheWAS(variant_id = "14_87978408_G_A") %>% otargen::plot_phewas(disease = TRUE)
  expected_2 <- otargen::pheWAS(variant_id = "rs55808324") %>% otargen::plot_phewas()
  expect_s3_class(expected_1, "ggplot")
  expect_s3_class(expected_2, "ggplot")
  expect_false(is.null(expected_1))
  expect_false(is.null(expected_2))
})
