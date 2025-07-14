test_that("test mousePhenotypesQuery works", {
  skip_on_cran()
  
  result <- mousePhenotypesQuery(ensemblId = "ENSG00000169174")
  
  expect_true(is.null(result) || is.data.frame(result) || is.list(result))
  if (!is.null(result) && length(result) > 0) {
    expect_true(all(c(
      "targetInModel" ,        "targetInModelMgiId",
      "modelPhenotypeId" ,     "modelPhenotypeLabel", 
      "modelPhenotypeClasses", "biologicalModels"  
    ) %in% names(result)))
  }
})
