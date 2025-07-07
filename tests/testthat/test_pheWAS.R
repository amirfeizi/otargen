test_that("test pheWAS works", {
  skip_on_cran()
  
  result <- pheWAS(variant_id = "1_154549918_C_A")
  
  expect_true(is.null(result) || is.data.frame(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true(all(c(
      "totalGWASStudies", "pval", "beta", "oddsRatio", 
      "nTotal", "study.studyId", "study.source", 
      "study.pmid", "study.pubDate", "study.traitReported", 
      "study.traitCategory"
    ) %in% colnames(result)))
  }
})
