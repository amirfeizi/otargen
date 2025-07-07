test_that("test topOverlappedStudies works", {
  skip_on_cran()
  
  result <- topOverlappedStudies(study_id = "GCST006614_3", pageindex = 0, pagesize = 5)
  
  expect_true(is.null(result) || is.data.frame(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true(any(grepl("study.studyId", colnames(result))))
    expect_true(any(grepl("topStudiesByLociOverlap.studyId", colnames(result))))
  }
})
