#' Get gwas credible set data for a variant in a study
#'
#' @param studyid is the Open Target Genetics generated id for gwas studies.
#' @param variantid is Open Target Genetics generated id for each variant in the databse.
#' @return a data frame of results from credible set of variants for a specific lead variant
#' @export





gwasCredibleSet <- function(studyid, variantid) {


  ## Query for GWAS study locus details

  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()
  otg_qry$query("credset_query", "query credsetQuery($studyId: String!, $variantId: String!){
  gwasCredibleSet(studyId: $studyId, variantId: $variantId) {
    tagVariant {
      id
    }
    beta
    postProb
    pval
  }
}")

  ## Execute the query
  variables <- list(studyId = studyid, variantId = variantid)
  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$credset_query, variables, flatten = TRUE))$data

  result <- purrr::flatten(result$gwasCredibleSet)
  return(result)
}
