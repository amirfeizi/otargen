#' Get gwas colocalisation data for a region
#'
#' @param chromosome
#' @param start
#' @param end
#'

gwasColocalisationForRegion <- function(chromosome, start, end) {

  ## Set up to query Open Targets Genetics API
  variables <- list(chromosome = chromosome, start = start, end = end)

  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()

  query <- "query gwascolforregquery($chromosome: String!, $start: Long!, $end: Long!){
  gwasColocalisationForRegion(chromosome: $chromosome, start: $start, end: $end) {
    leftVariant{
      id
    position
    chromosome
      rsId
    }
  leftStudy{
    studyId
    traitReported
  }
  rightVariant
  {
    id
    position
    chromosome
    rsId
  }
  rightStudy
  {
    studyId
    traitCategory
  }
  h3
  h4
  log2h4h3
  }
}"

  ## Execute the query
  otg_qry$query(name = "gwascolforreg_query", x = query)

  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$gwascolforreg_query, variables, flatten=TRUE))$data

  result_df <- result$gwasColocalisationForRegion %>% as.data.frame

  return(result_df)
}
