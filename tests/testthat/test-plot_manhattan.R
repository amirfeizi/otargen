test_that("test plot_manhattan works", {
  expected <- otargen::manhattan(study_id = "GCST003044") %>% otargen::plot_manhattan()
  expect_s3_class(expected, "ggplot")
  expect_false(is.null(expected))
})
