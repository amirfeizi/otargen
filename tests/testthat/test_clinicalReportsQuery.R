test_that("clinicalReportsQuery validates its input", {
  # Neither argument provided
  expect_error(clinicalReportsQuery(), "exactly one")
  # Both arguments provided
  expect_error(
    clinicalReportsQuery(chemblId = "CHEMBL941", reportIds = "nct00080665"),
    "exactly one"
  )
  # Empty / invalid values
  expect_error(clinicalReportsQuery(chemblId = ""), "exactly one")
  expect_error(clinicalReportsQuery(reportIds = character(0)), "exactly one")
})

test_that("clinicalReportsQuery returns a tidy tibble for a drug", {
  skip_on_cran()

  result <- clinicalReportsQuery(chemblId = "CHEMBL941")

  expect_true(is.null(result) || tibble::is_tibble(result))
  if (!is.null(result)) {
    expect_true(all(c(
      "reportId", "drugId", "title", "type", "clinicalStage",
      "trialPhase", "trialOverallStatus", "countries",
      "diseaseIds", "diseaseNames", "drugIds", "drugNames", "url"
    ) %in% colnames(result)))
    # Reports are de-duplicated by id
    expect_false(any(duplicated(result$reportId)))
    expect_true(all(result$drugId == "CHEMBL941"))
  }
})

test_that("clinicalReportsQuery fetches reports by id", {
  skip_on_cran()

  result <- clinicalReportsQuery(reportIds = "nct00080665")

  expect_true(is.null(result) || tibble::is_tibble(result))
  if (!is.null(result)) {
    expect_equal(nrow(result), 1L)
    expect_true(is.na(result$drugId[1]))
  }
})
