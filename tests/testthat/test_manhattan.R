test_that("test manhattan works", {
  skip_on_cran()
  
  result <- manhattan(study_id = "GCST90002357", pageindex = 0, pagesize = 5)
  
  expect_true(is.null(result) || tibble::is_tibble(result) || is.data.frame(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true(all(c(
      "pval_mantissa", "pval_exponent", "credible_set_size", "ld_set_size", 
      "total_set_size", "pval", "odds_ratio", "odds_ratio_ci_lower", 
      "odds_ratio_ci_upper", "beta", "beta_ci_lower", "beta_ci_upper", 
      "direction", "best_genes_score", "best_genes_gene_id", "best_genes_gene_symbol",
      "best_coloc_genes_score", "best_coloc_genes_gene_id", "best_coloc_genes_gene_symbol",
      "best_locus2genes_score", "best_locus2genes_gene_id", "best_locus2genes_gene_symbol",
      "variant_id", "variant_position", "variant_chromosome", "variant_rs_id"
    ) %in% colnames(result)))
  }
})