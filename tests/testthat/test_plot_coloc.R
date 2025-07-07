test_that("test plot_coloc works", {
  skip_on_cran()
  
  # Create minimal fake input data
  data <- data.frame(
    Trait_reported = "TestTrait",
    Study = "GCST000123",
    Lead_variant = "1_12345_A_T",
    Molecular_trait = "eQTL",
    Tissue = "Liver",
    Source = "OTG",
    `log2(H4/H3)` = 8
  )
  
  p <- plot_coloc(data, biobank = FALSE)
  expect_true(inherits(p, c("gg", "ggplot")))
})