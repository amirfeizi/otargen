test_that("test plot_l2g works", {
  skip_on_cran()
  
  # Fake example data
  data <- data.frame(
    yProbaModel = 0.9,
    yProbaDistance = 0.8,
    yProbaInteraction = 0.7,
    yProbaMolecularQTL = 0.6,
    yProbaPathogenicity = 0.5,
    gene_symbol = "GENE1",
    study.traitReported = "Trait X",
    study.traitEfos = "EFO_0003767",
    study.traitCategory = "disease",
    pval = 1e-8
  )
  
  p <- plot_l2g(data, disease_efo = "EFO_0003767")
  expect_true(inherits(p, c("gg", "ggplot")))
})
