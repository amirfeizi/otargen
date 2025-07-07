test_that("test compGenomicsQuery works", {
  skip_on_cran()
  
  result <- compGenomicsQuery(ensemblId = "ENSG00000169174")
  
  expect_true(is.null(result) || is.data.frame(result))
  if (!is.null(result)) {
    expect_true(all(c("speciesId", "speciesName", "homologyType", "isHighConfidence", 
                      "targetGeneId", "targetGeneSymbol", "queryPercentageIdentity", 
                      "targetPercentageIdentity") %in% colnames(result)))
  }
})


