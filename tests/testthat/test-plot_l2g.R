test_that("test plot_l2g works", {
  expected_1 <- otargen::studiesAndLeadVariantsForGeneByL2G(list("ENSG00000167207","ENSG00000096968","ENSG00000138821", "ENSG00000125255")) %>%
                otargen::plot_l2g()
  expected_2 <- otargen::studiesAndLeadVariantsForGeneByL2G(list("ENSG00000167207","ENSG00000096968","ENSG00000138821", "ENSG00000125255")) %>%
                otargen::plot_l2g(disease = "EFO_0003767")
  expect_s3_class(expected_1, "ggplot")
  expect_s3_class(expected_2, "ggplot")
  expect_false(is.null(expected_1))
  expect_false(is.null(expected_2))
})
