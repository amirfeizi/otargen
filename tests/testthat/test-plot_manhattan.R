test_that("test plot_manhattan works", {
  expected <- otargen::manhattan(studyid = "GCST003044") %>% otargen::plot_manhattan()
  expect_s3_class(expected, "ggplot")
  expect_false(is.null(expected))
})
