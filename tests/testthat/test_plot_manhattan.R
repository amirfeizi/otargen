test_that("test plot_manhattan works", {
  skip_on_cran()
  
  # Minimal fake data
  data <- data.frame(
    variant_position = c(1000, 2000, 3000),
    variant_chromosome = c("1", "1", "2"),
    pval = c(1e-9, 1e-6, 5e-8),
    variant_id = c("1_1000_A_T", "1_2000_G_C", "2_3000_T_G"),
    best_locus2genes_score = c(0.8, 0.7, 0.6),
    best_locus2genes_gene_symbol = c("GENE1", "GENE2", "GENE3")
  )
  
  p <- plot_manhattan(data)
  expect_true(inherits(p, c("gg", "ggplot")))
})