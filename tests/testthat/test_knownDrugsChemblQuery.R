test_that("test knownDrugsChemblQuery works", {
  skip_on_cran()
  
  result <- knownDrugsChemblQuery(
    chemblId = "CHEMBL1016"
  )
  
  expect_true(is.null(result) || tibble::is_tibble(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true(all(c(
      "maxClinicalStage", "disease.id", "disease.name",
      "drugId", "indicationsCount"
    ) %in% colnames(result)))
  }
})