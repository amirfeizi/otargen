test_that("test qtlColocalisationVariantQuery works", {
  skip_on_cran()
  
  result <- qtlColocalisationVariantQuery(
    study_id = "GCST90002357",
    variant_id = "1_154119580_C_A"
  )
  
  expect_true(is.null(result) || is.data.frame(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true(all(c(
      "qtlStudyName", "phenotypeId", "gene.id", "gene.symbol",
      "tissue.name", "indexVariant.id", "indexVariant.rsId",
      "beta", "h4", "h3", "log2h4h3"
    ) %in% colnames(result)))
  }
})