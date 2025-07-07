test_that("test run_custom_query works", {
  skip_on_cran()
  
  query <- "
    query searchVariants($queryString: String!) {
      search(queryString: $queryString) {
        variants {
          id
        }
      }
    }"
  
  variables <- list(queryString = "rs2494663")
  
  result <- run_custom_query(
    variableList = variables,
    query = query,
    query_name = "searchVariants"
  )
  
  expect_true(is.null(result) || is.list(result))
  if (!is.null(result)) {
    expect_true("search.variants.id" %in% names(unlist(result)))
  }
})