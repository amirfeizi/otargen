#' Gives out top studies ordered by loci overlap.
#'
#' @param studyid which links the top loci with a trait. Formet: String
#' @param pageIndex pagination index >= 0. Index of the current page.
#' @param pageSize pagination size > 0. No. of records in a page. Default: 20
#' @returns A data frame with top studies.
#' @examples
#' topOverlappedStudies("GCST006614_3")
#' topOverlappedStudies("NEALE2_6177_1", pageindex=1, pagesize=50)
#'
#' @export
#'

topOverlappedStudies <- function(studyid, pageindex=0, pagesize=20) {

  ## Set up to query Open Targets Genetics API

  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()

  variables <- list(studyId = studyid, pageIndex = pageindex, pageSize=pagesize)

  query <- "query topoverlapstudiesquery($studyId: String!, $pageIndex: Int!, $pageSize:Int!){
  topOverlappedStudies(studyId: $studyId, pageIndex: $pageIndex, pageSize: $pageSize) {
    study {
    studyId
    }
    topStudiesByLociOverlap{
    studyId
    study{
      studyId
    }
    numOverlapLoci
  }
  }
}"

  ## Execute the query
  otg_qry$query(name = "topoverlapstudies_query", x = query )

  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$topoverlapstudies_query, variables, flatten=TRUE))$data

  result_df <- result$topOverlappedStudies %>% as.data.frame

  return(result_df)
}
