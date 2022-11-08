#' GWAS Regional Association of a study.
#'
#' @param studyid is the Open Target Genetics generated id for gwas studies.
#' @param chromosome
#' @param start
#' @param end
#' @export

gwasRegional <- function(studyid, chromosome, start, end) {

  ## Set up to query Open Targets Genetics API

  variables <- list(studyId = studyid, chromosome = chromosome, start = start, end = end)

  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()

  query <- "query gwasregionalquery($studyId: String!, $chromosome: String! $start: Long!, $end: Long!){
  gwasRegional(studyId: $studyId, chromosome: $chromosome, start: $start, end: $end) {
     variant{
    id
    chromosome
    position
  }
  pval

  }
}"


  ## Execute the query

  otg_qry$query(name = "gwasregional_query", x =  query)

  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$gwasregional_query, variables), flatten=TRUE)$data

  result_df <- result$gwasRegional %>% as.data.frame

  return(result_df)
}
