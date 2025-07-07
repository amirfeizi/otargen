test_that("test qtlCredibleSet works", {
  skip_on_cran()
  
  result <- qtlCredibleSet(
    study_id = "Braineac2",
    variant_id = "1_55053079_C_T",
    gene = "ENSG00000169174",
    biofeature = "SUBSTANTIA_NIGRA"
  )
  
  expect_true(is.null(result) || is.data.frame(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true(all(c(
      "tagVariant.id", "tagVariant.rsId", "pval", "se", "beta",
      "postProb", "MultisignalMethod", "logABF", "is95", "is99"
    ) %in% colnames(result)))
  }
})