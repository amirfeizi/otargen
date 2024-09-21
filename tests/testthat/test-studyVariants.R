test_that("test studyVariants works", {
  skip_on_cran()
  expected <- otargen::studyVariants(study_id = "GCST003155")
  expect_type(expected, "list")
  expect_false(is.null(length(expected)))
})
