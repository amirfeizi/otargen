test_that("colocalisationsForGene returns a data frame with expected columns", {
  skip_on_cran()
  
  # Use a known ENSG ID with some data (you can replace with your own)
  ensg_id <- "ENSG00000169174"
  
  result <- colocalisationsForGene(genes = ensg_id)
  
  # Should either return empty data.frame or a tibble
  expect_true(is.data.frame(result))
  
  if (nrow(result) > 0) {
    expected_cols <- c(
      "Study", "Trait_reported", "Lead_variant", "Molecular_trait", "Gene_symbol",
      "Tissue", "Source", "H3", "H4", "log2(H4/H3)", "Title", "Author",
      "Has_sumstats", "numAssocLoci", "nInitial cohort",
      "study_nReplication", "study_nCases", "Publication_date", "Journal", "Pubmed_id"
    )
    
    expect_true(all(expected_cols %in% colnames(result)))
    
    # Check types for a few numeric columns
    expect_true(is.numeric(result$H3) || is.logical(result$H3))
    expect_true(is.numeric(result$H4) || is.logical(result$H4))
    expect_true(is.numeric(result$`log2(H4/H3)`) || is.logical(result$`log2(H4/H3)`))
  }
})

test_that("colocalisationsForGene works with a gene symbol", {
  skip_on_cran()
  
  gene_symbol <- "TP53"
  
  result <- colocalisationsForGene(genes = gene_symbol)
  
  expect_true(is.data.frame(result))
  
  if (nrow(result) > 0) {
    expect_true("Gene_symbol" %in% colnames(result))
  }
})

test_that("colocalisationsForGene fails with a fake gene", {
  skip_on_cran()
  
  expect_error(
    colocalisationsForGene(genes = "NOT_A_REAL_GENE"),
    regexp = "Please provide Ensemble gene ID or gene name"
  )
})
