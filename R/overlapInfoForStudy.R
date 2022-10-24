#' Get overlap info for a study with given list of studies
#'
#' @param studyid is the Open Target Genetics generated id for gwas studies.
#' @param studyids is a list of Open Target Genetics generated id for gwas studies.
#' @return a data frame of results containing the overlap info
#' @export
#'

overlapInfoForStudy <- function(studyid, studyids=list()) {


  ## Set up to query Open Targets Genetics API

  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()
  otg_qry$query("overlapinfostudy_query", "query overlapinfostudyquery($studyId: String!, $studyIds: [String!]!){
  overlapInfoForStudy(studyId: $studyId, studyIds: $studyIds) {
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
  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$overlapinfostudy_query, variables), flatten=TRUE)$data
  result <- tibble(place = result)

  final_df <- result %>% unnest_wider(place) %>% select(overlappedVariantsForStudies) %>% unnest(overlappedVariantsForStudies) %>%
    hoist(overlaps, variantIdA = 'variantIdA', variantIdB = 'variantIdB', overlapAB = 'overlapAB', distinctA = 'distinctA', distinctB = 'distinctB') %>%
    unnest(c(variantIdA, variantIdB, overlapAB, distinctA, distinctB)) %>% as.data.frame

  return(final_df)
}
