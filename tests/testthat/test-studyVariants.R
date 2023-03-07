test_that("test studyVariants works", {
  expected <- otargen::studyVariants(studyid = "GCST003155")
  expect_type(expected, "list")
  expect_false(is.null(length(expected)))
})
