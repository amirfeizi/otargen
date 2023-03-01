test_that("test genesForVariant works", {
    expected_1 <- otargen::genesForVariant(variantid = "1_154453788_C_T")
    expected_2 <- otargen::genesForVariant(variantid = "rs55808324")
    expect_type(expected_1, "list")
    expect_type(expected_2, "list")
    expect_false(is.null(length(expected_1)))
    expect_false(is.null(length(expected_2)))
})
