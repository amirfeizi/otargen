test_that("test the gwasColocalisation works", {
  expected <- otargen::gwasColocalisation(studyid = "GCST006614_3" ,variantid = "1_55058182_G_A")
  expect_s3_class(expected, "data.frame")
  expect_false(is.null(dim(expected)))

})
