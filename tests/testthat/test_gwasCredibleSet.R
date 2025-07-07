test_that("test gwasCredibleSet works", {
  skip_on_cran()
  
  result <- gwasCredibleSet(study_id = "GCST90002357", variant_id = "rs2494663")
  
  expect_true(is.null(result) || tibble::is_tibble(result) || is.data.frame(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true(all(c("tagVariant.id", "tagVariant.rsId", "beta", "postProb",
                      "pval", "se", "MultisignalMethod", "logABF", "is95", "is99") %in% colnames(result)))
  }
})