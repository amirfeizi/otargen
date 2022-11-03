test_that("test gwasCredibleSet works", {
  expected <- otargen::gwasCredibleSet(studyid = "GCST006614_3" ,variantid = "1_55058182_G_A")
  expect_s3_class(expected, "data.frame")
  expect_false(is.null(dim(expected)))
})
