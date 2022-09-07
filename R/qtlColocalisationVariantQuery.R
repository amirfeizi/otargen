#' Get Colocalization data for a variant in specific study
#'
#'
#' @param studyid is the Open Target Genetics generated id for gwas studies.
#' @param variantid is Open Target Genetics generated id for each variant in the databse.

qtlColocalisationVariantQuery <- function(studyid, variantid) {


  ## Query for QTL colocalisation

  otg_cli <- GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- Query$new()
  otg_qry$query(
    'qtl_query',
    'query qtlColocalisationVariantQuery($studyId: String!, $variantId: String!) {
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
}'
  )

  variables = list(studyId = studyid, variantId = variantid)
  result <-
    fromJSON(otg_cli$exec(otg_qry$queries$qtl_query, variables, flatten = TRUE))$data
  l2g_result <- result$qtlColocalisation
  return(l2g_result)

}
