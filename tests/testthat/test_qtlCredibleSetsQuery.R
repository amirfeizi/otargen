test_that("test qtlCredibleSetsQuery works", {
  skip_on_cran()
  
  result <- qtlCredibleSetsQuery(
    variantId = "19_10352442_G_C",
    size = 2,
    index = 0
  )
  
  expect_true(is.null(result) || tibble::is_tibble(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true("studyLocusId" %in% colnames(result))
    expect_true("variantId" %in% colnames(result))
    expect_true("referenceAllele" %in% colnames(result))
    expect_true("alternateAllele" %in% colnames(result))
  }
})
