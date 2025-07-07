test_that("test variantEffectQuery works", {
  skip_on_cran()
  
  result <- variantEffectQuery(variantId = "4_1804392_G_A")
  
  expect_true(is.null(result) || tibble::is_tibble(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true(all(c(
      "method", "assessment", "score", "assessmentFlag",
      "normalisedScore", "variantId"
    ) %in% colnames(result)))
  }
})
