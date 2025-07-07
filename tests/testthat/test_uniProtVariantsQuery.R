test_that("test uniProtVariantsQuery works", {
  skip_on_cran()
  
  result <- uniProtVariantsQuery(variantId = "4_1804392_G_A")
  
  expect_true(is.null(result) || tibble::is_tibble(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true(all(c(
      "targetFromSourceId", "confidence", "diseaseFromSource",
      "disease.id", "disease.name", "variantId"
    ) %in% colnames(result)))
  }
})