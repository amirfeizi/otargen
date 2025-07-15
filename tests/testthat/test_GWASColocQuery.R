test_that("test safetyQuery works", {
  skip_on_cran()
  
  colocalisation_data <- gwasColocalisation(study_locus_id = "5a86bfd40d2ebecf6ce97bbe8a737512", size = 500, index = 0)
  
  expect_true(is.null(colocalisation_data) || tibble::is_tibble(colocalisation_data))
  if (!is.null(colocalisation_data) && nrow(colocalisation_data) > 0) {
    expect_true(all(c(
      "study.studyId"   ,             "study.projectId"     ,         "study.traitReported" 
    ) %in% names(colocalisation_data)))
  }
})
