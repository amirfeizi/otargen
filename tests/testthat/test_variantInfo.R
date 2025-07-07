test_that("test variantInfo works", {
  skip_on_cran()
  
  result <- variantInfo(variant_id = "rs2494663")
  
  expect_true(is.null(result) || is.data.frame(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true(all(c(
      "chromosome", "position", "refAllele", "altAllele",
      "rsId", "mostSevereConsequence", "gnomadAFR", "gnomadEAS"
    ) %in% colnames(result)))
  }
})
