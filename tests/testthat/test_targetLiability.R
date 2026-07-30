test_that("targetLiability validates its input", {
  expect_error(targetLiability(), "ensgId")
  expect_error(targetLiability(NULL), "ensgId")
  expect_error(targetLiability(""), "ensgId")
})

test_that("targetLiability returns a scored tibble", {
  skip_on_cran()

  result <- targetLiability(ensgId = "ENSG00000141510", verbose = FALSE)

  expect_true(is.null(result) || tibble::is_tibble(result))
  if (!is.null(result)) {
    expect_equal(nrow(result), 1L)
    expect_true(all(c(
      "geneId", "liability_score", "liability_category",
      "recommendation", "rationale"
    ) %in% colnames(result)))
    # Score is bounded in [0, 1]
    expect_true(is.na(result$liability_score) ||
                  (result$liability_score >= 0 & result$liability_score <= 1))
    expect_true(result$liability_category %in% c("Low", "Moderate", "High"))
  }
})

test_that("targetLiability can skip the essentiality call", {
  skip_on_cran()

  result <- targetLiability(ensgId = "ENSG00000169174",
                            include_essentiality = FALSE, verbose = FALSE)
  expect_true(is.null(result) || tibble::is_tibble(result))
  if (!is.null(result)) {
    expect_true(is.na(result$essentiality_score))
  }
})
