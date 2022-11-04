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
  result <- dplyr::tibble(place = result)

<<<<<<< HEAD
  final_df <- result %>% tidyr::unnest_wider(place) %>% dplyr::select(overlappedVariantsForStudies) %>% tidyr::unnest(overlappedVariantsForStudies, keep_empty=TRUE) %>%
    tidyr::hoist(overlaps, variantIdA = 'variantIdA', variantIdB = 'variantIdB', overlapAB = 'overlapAB', distinctA = 'distinctA', distinctB = 'distinctB') %>%
    tidyr::unnest(c(variantIdA, variantIdB, overlapAB, distinctA, distinctB), keep_empty=TRUE) %>% as.data.frame
=======
  final_df <- result %>% tidyr::unnest_wider(place) %>% dplyr::select(overlappedVariantsForStudies) %>% tidyr::unnest(overlappedVariantsForStudies) %>%
    tidyr::hoist(overlaps, variantIdA = 'variantIdA', variantIdB = 'variantIdB', overlapAB = 'overlapAB', distinctA = 'distinctA', distinctB = 'distinctB') %>%
    tidyr::unnest(c(variantIdA, variantIdB, overlapAB, distinctA, distinctB)) %>% base::as.data.frame()
>>>>>>> 4309f197b23eaf65b35e242fef6e3aa7b7f93493

  return(final_df)
}
