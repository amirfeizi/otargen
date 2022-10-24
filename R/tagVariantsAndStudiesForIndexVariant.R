#' Gives out tagVariants and studies for a given index variant.
#'
#' @param variantid is the Open Target Genetics generated id for each variants.
#' @param pageIndex
#' @param pageSize
#' @export

tagVariantsAndStudiesForIndexVariant <- function(variantid, pageindex=0, pagesize=0) {

  ## Set up to query Open Targets Genetics API

  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()
  otg_qry$query("tagVariantsAndStudiesForIndexVariant_query", "query tagVariantsAndStudiesForIndexVariantquery($variantId: String!, $pageIndex: Int!, $pageSize:Int!){
  tagVariantsAndStudiesForIndexVariant(variantId: $variantId, pageIndex: $pageIndex, pageSize: $pageSize) {
    associations{
  tagVariant{
        id
    chromosome
    rsId
    position
      }
      study{
        studyId
        traitReported
      }
      pval
      pvalMantissa
      pvalExponent
    nTotal
    nCases
    overallR2
    afr1000GProp
    amr1000GProp
    eas1000GProp
    eur1000GProp
    sas1000GProp
      oddsRatio
    oddsRatioCILower
    oddsRatioCIUpper
      posteriorProbability
      beta
    betaCILower
    betaCIUpper
      direction
      log10Abf
  }
  }
}")

  ## Execute the query
  variables <- list(variantId = variantid, pageIndex = pageindex, pageSize=pagesize)

  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$tagVariantsAndStudiesForIndexVariant_query, variables, flatten=TRUE))$data

  result_df <- result$tagVariantsAndStudiesForIndexVariant$associations %>% as.data.frame

  return(result_df)
}