test_that("test gwasColocalizationForRegion works", {
 expected <- otargen::gwasColocalisationForRegion(chromosome = "1", start = 55058000  , end = 5505900 )
 expect_s3_class(expected, "data.frame")
 expect_false(is.null(dim(expected)))
})
