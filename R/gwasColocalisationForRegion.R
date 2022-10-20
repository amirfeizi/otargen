#' Get gwas colocalisation data for a region
#'
#' @param chromosome
#' @param start
#' @param end
#'

gwasColocalisationForRegion <- function(chromosome, start, end) {

  ## Set up to query Open Targets Genetics API

  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()
  otg_qry$query("gwascolforreg_query", "query gwascolforregquery($chromosome: String!, $start: Long!, $end: Long!){
  gwasColocalisationForRegion(chromosome: $chromosome, start: $start, end: $end) {
    leftVariant{
      id
      rsId
    }
  leftStudy{
    studyId
  }
  rightVariant
  {
    id
    rsId
  }
  rightStudy
  {
    studyId
  }
  h3
  h4
  log2h4h3
  }
}")

  ## Execute the query
  variables <- list(chromosome = chromosome, start = start, end = end)
  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$gwascolforreg_query, variables, flatten=TRUE))$data

  result_df <- result %>% as.data.frame

  return(result_df)
}
