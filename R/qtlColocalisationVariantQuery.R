#' Get Colocalization data for a variant in specific study
#'
#'
#' @param studyid is the Open Target Genetics generated id for gwas studies.
#' @param variantid is Open Target Genetics generated id for each variant in the databse.
#' @return a data frame of the colocalization information for a lead variant
#' @export

qtlColocalisationVariantQuery <- function(studyid, variantid) {


  ## Query for QTL colocalisation
  variables <- list(studyId = studyid, variantId = variantid)

  query <- "query qtlColocalisationVariantQuery($studyId: String!, $variantId: String!) {
  qtlColocalisation(studyId: $studyId, variantId: $variantId){
    qtlStudyName
    phenotypeId
    gene {
      id
      symbol
    }
    tissue {
      name
    }
    indexVariant {
      id
    }
    beta
    h4
  }
}"

  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()

  # execute the query
  otg_qry$query(name = "qtl_query", x = query)

  result <-
    jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$qtl_query, variables, flatten = TRUE))$data
  l2g_result <- result$qtlColocalisation
  return(l2g_result)
}
