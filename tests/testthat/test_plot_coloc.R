test_that("test plot_coloc works", {
  skip_on_cran()
  
  # Create minimal fake input data
  data <- colocalisationsForGene(genes = "ENSG00000169174")
  
  p <- plot_coloc(data, biobank = FALSE)
  expect_true(inherits(p, c("gg", "ggplot")))
})
