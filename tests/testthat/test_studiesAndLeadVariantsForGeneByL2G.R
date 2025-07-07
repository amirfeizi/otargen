test_that("test studiesAndLeadVariantsForGeneByL2G works", {
  skip_on_cran()
  
  result <- studiesAndLeadVariantsForGeneByL2G(
    gene = "ENSG00000169174",
    l2g = 0.2
  )
  
  expect_true(is.null(result) || is.data.frame(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true(all(c(
      "yProbaModel", "yProbaDistance", "yProbaInteraction", "yProbaMolecularQTL",
      "yProbaPathogenicity", "pval", "study.studyId", "study.traitReported",
      "variant.id", "variant.rsId", "gene_symbol", "ensembl_id"
    ) %in% colnames(result)))
  }
})
