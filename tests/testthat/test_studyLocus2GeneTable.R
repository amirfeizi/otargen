test_that("test studyLocus2GeneTable works", {
  skip_on_cran()
  
  result <- studyLocus2GeneTable(
    study_id = "GCST90002357",
    variant_id = "1_154119580_C_A"
  )
  
  expect_true(is.null(result) || tibble::is_tibble(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true(all(c(
      "study.studyId", "variant.id", "variant.rsId",
      "yProbaDistance", "yProbaModel", "yProbaMolecularQTL",
      "yProbaPathogenicity", "yProbaInteraction", "hasColoc",
      "distanceToLocus", "gene.id", "gene.symbol"
    ) %in% colnames(result)))
  }
})
