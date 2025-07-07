test_that("test overlapInfoForStudy works", {
  skip_on_cran()
  
  result <- overlapInfoForStudy(
    study_id = "GCST90002357",
    study_ids = list("GCST90025975", "GCST90025962")
  )
  
  expect_type(result, "list")
  expect_true(all(c("overlap_info", "variant_intersection_set") %in% names(result)))
  
  if (!is.null(result$overlap_info) && nrow(result$overlap_info) > 0) {
    expect_true(all(c(
      "studyId", "traitReported", "traitCategory",
      "variantIdA", "variantIdB", "overlapAB",
      "distinctA", "distinctB", "study.studyId",
      "study.traitReported", "study.traitCategory"
    ) %in% colnames(result$overlap_info)))
  }
  
  expect_true(is.null(result$variant_intersection_set) || is.character(result$variant_intersection_set))
})