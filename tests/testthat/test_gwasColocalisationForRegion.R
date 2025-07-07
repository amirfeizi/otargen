test_that("test gwasColocalisationForRegion works", {
  skip_on_cran()
  
  result <- gwasColocalisationForRegion(chromosome = "1", start = 153992685, end = 154155116)
  
  expect_true(is.null(result) || tibble::is_tibble(result) || is.data.frame(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true(all(c("leftVariant.id", "leftVariant.position", "leftVariant.chromosome",
                      "leftVariant.rsId", "leftStudy.studyId", "leftStudy.traitReported",
                      "leftStudy.traitCategory", "rightVariant.id", "rightVariant.position",
                      "rightVariant.chromosome", "rightVariant.rsId", "rightStudy.studyId",
                      "rightStudy.traitReported", "rightStudy.traitCategory",
                      "h3", "h4", "log2h4h3") %in% colnames(result)))
  }
})