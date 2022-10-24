#' Gives out top studies ordered by loci overlap.
#'
#' @param studyid is the Open Target Genetics generated id for gwas studies.
#' @param pageIndex
#' @param pageSize
#' @export

topOverlappedStudies <- function(studyid, pageindex=0, pagesize=0) {

  ## Set up to query Open Targets Genetics API

  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()
  otg_qry$query("topoverlapstudies_query", "query topoverlapstudiesquery($studyId: String!, $pageIndex: Int!, $pageSize:Int!){
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
}")

  ## Execute the query
  variables <- list(studyId = studyid, pageIndex = pageindex, pageSize=pagesize)

  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$topoverlapstudies_query, variables, flatten=TRUE))$data

  result_df <- result$topOverlappedStudies %>% as.data.frame

  return(result_df)
}
