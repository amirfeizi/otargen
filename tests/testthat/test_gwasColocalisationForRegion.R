test_that("test gwasColocalisationForRegion works", {
  skip_on_cran()
  
  result <- gwasColocalisationForRegion(chromosome = "1", start = 153992685, end = 154155116)
  
  expect_true(is.null(result) || tibble::is_tibble(result) || is.data.frame(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true(all(c("leftVariant" , "leftStudy","rightVariant",
                      "rightStudy" ,"h3", 
                      "h4", "log2h4h3" ) %in% colnames(result)))
  }
})