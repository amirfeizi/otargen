#' Get gwas colocalisation for a variant in a study
#'
#' @param studyid is the Open Target Genetics generated id for gwas studies.
#' @param variantid is Open Target Genetics generated id for each variant in the database.
#' @return A data frame of the studies which colocalise with the input variant and study.
#' @examples
#' gwasColocalisation("GCST90002357", "1_154119580_C_A")
#' @export
#'

gwasColocalisation <- function(studyid, variantid) {

  variables <- list(studyId = studyid, variantId = variantid)

  ## Set up to query Open Targets Genetics API
  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()

  query <- 'query gwascolquery($studyId: String!, $variantId: String!){
    gwasColocalisation(studyId: $studyId, variantId: $variantId){
    indexVariant{
    id
    position
    chromosome
    rsId
  }
  study{
    studyId
    traitReported
  }
  beta
  h3
  h4
  log2h4h3
}
}'


  ## Execute the query
  otg_qry$query(name = 'gwascol_query' , query)

  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$gwascol_query,variables), flatten = TRUE)$data

  result_df <- result$gwasColocalisation %>% as.data.frame

  return(result_df)

}
