test_that("test interactionsQuery works", {
  skip_on_cran()
  
  result <- interactionsQuery(
    ensgId = "ENSG00000141510",
    sourceDatabase = "intact",
    index = 0,
    size = 5
  )
  
  expect_true(is.null(result) || tibble::is_tibble(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true(all(c("intA", "intABiologicalRole", "targetA.id", "targetA.approvedSymbol",
                      "speciesA.mnemonic", "intB", "intBBiologicalRole",
                      "targetB.id", "targetB.approvedSymbol", "speciesB.mnemonic",
                      "score", "count", "sourceDatabase") %in% colnames(result)))
  }
})