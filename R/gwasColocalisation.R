#' Get gwas colocalisation for a variant in a study
#'
#' @param studyid is the Open Target Genetics generated id for gwas studies.
#' @param variantid is Open Target Genetics generated id for each variant in the database.
#' @return a data frame of the studies which colocalise with the input variant and study
#' @export

gwasColocalisation <- function(studyid, variantid) {
  ## Set up to query Open Targets Genetics API
  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()

  otg_qry$query('gwascol_query' , 'query gwascolquery($studyId: String!, $variantId: String!){
    gwasColocalisation(studyId: $studyId, variantId: $variantId){
    indexVariant{
    id
    position
    altAllele
    rsId
  }
  study{
    traitReported
    pubAuthor
    pmid
    nCases
  }
  beta
  h3
  h4
  log2h4h3
}
}')

  ## Execute the query
  variables <- list(studyId = studyid, variantId = variantid)
  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$gwascol_query,variables), flatten = TRUE)

  print (result)

}
