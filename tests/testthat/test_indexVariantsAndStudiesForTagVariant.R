
test_that("test indexVariantsAndStudiesForTagVariant works", {
  skip_on_cran()
  
  result <- indexVariantsAndStudiesForTagVariant(
    variant_id = "rs12740374",
    pageindex = 0,
    pagesize = 5
  )
  
  expect_true(is.null(result) || tibble::is_tibble(result) || is.data.frame(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true(all(c(
      "index_variant.id", "index_variant.rs_id", 
      "study.study_id", "study.trait_reported", "study.trait_category",
      "pval", "pval_mantissa", "pval_exponent", "n_total", "n_cases",
      "overall_r2", "afr1000g_prop", "amr1000g_prop", "eas1000g_prop",
      "eur1000g_prop", "sas1000g_prop", "log10abf", "posterior_probability",
      "odds_ratio", "odds_ratio_ci_lower", "odds_ratio_ci_upper",
      "beta", "beta_ci_lower", "beta_ci_upper", "direction"
    ) %in% colnames(result)))
  }
})