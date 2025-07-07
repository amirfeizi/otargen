test_that("test knownDrugsChemblQuery works", {
  skip_on_cran()
  
  result <- knownDrugsChemblQuery(
    chemblId = "CHEMBL1016",
    size = 5
  )
  
  expect_true(is.null(result) || tibble::is_tibble(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true(all(c(
      "phase", "status", "urls", "disease.id", "disease.name",
      "target.id", "target.approvedName", "target.approvedSymbol",
      "drugId", "knownDrugsCount", "cursor"
    ) %in% colnames(result)))
  }
})