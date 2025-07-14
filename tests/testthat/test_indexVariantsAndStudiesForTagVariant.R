
test_that("test indexVariantsAndStudiesForTagVariant works", {
  skip_on_cran()
  
  result <- indexVariantsAndStudiesForTagVariant(
    variant_id = "rs12740374",
    pageindex = 0,
    pagesize = 5
  )
  
  expect_true(is.null(result) || tibble::is_tibble(result) || is.data.frame(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true(all(c(
      "study", "pval"  
    ) %in% names(result)))
  }
})
