#' Get locus data table of a variant around a gene
#'
#' @param studyid is the Open Target Genetics generated id for gwas studies.
#' @param variantid is Open Target Genetics generated id for each variant in the databse.
#' @return a datframe with summary stats of the study and datatable of the various calculated scores and features for any lead variant
#' @export

studyLocus2GeneTable <- function(studyid, variantid) {


  ## Set up to query Open Targets Genetics API
  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()

  ## Query for GWAS study locus details
  otg_qry$query("l2g_query", "query l2gQuery($studyId: String!, $variantId: String!){
    studyInfo(studyId: $studyId){
    numAssocLoci
    ancestryInitial
    nTotal
    nCases
    pubAuthor
  }
  studyLocus2GeneTable(studyId: $studyId, variantId: $variantId){
    rows {
      gene {
        id
        symbol
      }
      hasColoc
      yProbaModel
      distanceToLocus
    }
  }
}")

  ## Execute the query
  variables <- list(studyId = studyid, variantId = variantid)
  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$l2g_query, variables, flatten = TRUE))$data

  return(result)
}
