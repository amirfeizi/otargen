#' Get overlap info for a study with given list of studies
#'
#' @param studyid is the Open Target Genetics generated id for gwas studies.
#' @param studyids is a list of Open Target Genetics generated id for gwas studies.
#' @return a data frame of results containing the overlap info
#'

overlapInfoForStudy <- function(studyid, studyids=list()) {


  ## Set up to query Open Targets Genetics API

  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()
  otg_qry$query("overlapinfo_query", "query overlapinfoquery($studyId: String!, $studyIds: [String!]!){
  overlapInfoForStudy(studyId: $studyId, studyIds: $studyIds) {
    study{
    studyId
  }
  overlappedVariantsForStudies{
    overlaps{
      variantIdA
      variantIdB
      overlapAB
      distinctA
      distinctB
    }
    study{
      studyId
    }
  }
  variantIntersectionSet
  }
}")

  ## Execute the query
  variables <- list(studyId = studyid, studyIds = studyids)
  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$overlapinfo_query, variables, flatten=TRUE))$data

  result_df <- result %>% as.data.frame
  result_df = dplyr::select(result_df, -c('overlapInfoForStudy.overlappedVariantsForStudies.overlaps'))

  overlaps <- result$overlapInfoForStudy$overlappedVariantsForStudies$overlaps

  return(result)
}
