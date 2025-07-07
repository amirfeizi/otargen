test_that("test safetyQuery works", {
  skip_on_cran()
  
  result <- safetyQuery(ensgId = "ENSG00000141510")
  
  expect_true(is.null(result) || tibble::is_tibble(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true(all(c(
      "event", "eventId", "biosamples.cellFormat",
      "biosamples.cellLabel", "biosamples.tissueLabel",
      "biosamples.tissueId", "effects.dosing", "effects.direction",
      "studies.name", "studies.type", "studies.description",
      "datasource", "literature", "url"
    ) %in% colnames(result)))
  }
})
