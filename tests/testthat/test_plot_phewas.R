test_that("test plot_phewas works", {
  skip_on_cran()
  
  # Minimal fake data
  data <- data.frame(
    study.traitCategory = "disease",
    study.traitReported = "SomeTrait",
    pval = 1e-10,
    beta = 0.5,
    study.source = "GCST"
  )
  
  p <- plot_phewas(data, disease = TRUE)
  expect_true(inherits(p, c("gg", "ggplot")))
})