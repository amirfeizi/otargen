test_that("test overlapinfoForStudy works", {
  expected <- otargen::overlapInfoForStudy(study_id = "GCST90025954" , study_ids = c("GCST006612","GCST010245" ,"GCST90038690"))
  expect_type(expected, "list")
  expect_false(is.null(length(expected)))
})
