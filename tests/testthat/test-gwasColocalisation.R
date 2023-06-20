test_that("test the gwasColocalisation works", {
  expected <- otargen::gwasColocalisation(study_id = "GCST90002357" ,variant_id = "1_154119580_C_A")
  expect_s3_class(expected, "data.frame")
  expect_false(is.null(dim(expected)))

})
