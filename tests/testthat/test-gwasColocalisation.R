test_that("test the gwasColocalisation works", {
  expected <- otargen::gwasColocalisation(studyid = "GCST90002357" ,variantid = "1_154119580_C_A")
  expect_s3_class(expected, "data.frame")
  expect_false(is.null(dim(expected)))

})
