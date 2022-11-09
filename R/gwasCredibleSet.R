#' Get gwas credible set data for a variant in a study
#'
#' @param studyid is the Open Target Genetics generated id for gwas studies.
#' @param variantid is Open Target Genetics generated id for each variant in the databse.
#' @return A data frame of results from credible set of variants for a specific lead variant.
#' @examples
#' gwasCredibleSet("GCST90002357", "1_154119580_C_A")
#' @export
#'





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
    se
    MultisignalMethod
    logABF
    is95
    is99
  }
}")

  ## Execute the query
  variables <- list(studyId = studyid, variantId = variantid)
  #variables <- list(studyId = "FINNGEN_R5_G6_AD_WIDE_EXMORE", variantId = "19_44908822_C_T")
  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$credset_query,
                                            variables, flatten = TRUE))$data

  result <- result$gwasCredibleSet %>% as.data.frame


  return(result)
}
