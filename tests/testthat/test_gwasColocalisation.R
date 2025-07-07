test_that("test gwasColocalisation works", {
  skip_on_cran()
  
  result <- gwasColocalisation(study_id = "GCST90002357", variant_id = "1_154119580_C_A")
  
  expect_true(is.null(result) || tibble::is_tibble(result) || is.data.frame(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true(all(c("study.studyId", "study.traitReported", "study.traitCategory",
                      "indexVariant.id", "indexVariant.position",
                      "indexVariant.chromosome", "indexVariant.rsId",
                      "beta", "h3", "h4", "log2h4h3") %in% colnames(result)))
  }
})