test_that("test geneOntologyQuery works", {
  skip_on_cran()
  
  result <- geneOntologyQuery(ensgId = "ENSG00000141510")
  
  expect_true(is.null(result) || tibble::is_tibble(result))
  if (!is.null(result)) {
    expect_true(all(c("term.id", "term.name", "aspect", "evidence", "geneProduct", "source") %in% colnames(result)))
  }
})