test_that("test tagVariantsAndStudiesForIndexVariant works", {
  skip_on_cran()
  
  result <- tagVariantsAndStudiesForIndexVariant(
    variant_id = "1_109274968_G_T",
    pageindex = 0,
    pagesize = 2
  )
  
  expect_true(is.null(result) || is.data.frame(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true(all(c(
      "tagVariant", "study" , "pval", "pvalMantissa",  "pvalExponent" 
    ) %in% colnames(result)))
  }
})
