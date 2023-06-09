#' Retrieves the top studies having overlap with the given input study.
#'
#' Fetches the top associated studies for a given study id ordered by loci overlap.
#'
#' @param studyid String: Open Target Genetics generated id for GWAS study.
#' @param pageindex Int: Index of the current page, pagination index >= 0.
#' @param pagesize Int: No. of records in a page, pagination size > 0.
#'
#' @returns Returns a data frame with top studies containing the following columns:
#'
#' \enumerate{
#' \item study.studyId
#' \item study.traitReported
#' \item study.traitCategory
#' \item topStudiesByLociOverlap.studyId
#' \item topStudiesByLociOverlap.study.studyId
#' \item topStudiesByLociOverlap.study.traitReported
#' \item topStudiesByLociOverlap.study.traitCategory
#' \item topStudiesByLociOverlap.numOverlapLoci
#' }
#'
#' @examples
#' \dontrun{
#' otargen::topOverlappedStudies(studyid = "GCST006614_3")
#' otargen::topOverlappedStudies(studyid = "NEALE2_6177_1", pageindex = 1, pagesize = 50)
#'}
#' @importFrom magrittr %>%
#' @export
#'
#'
#'
topOverlappedStudies <- function(studyid, pageindex = 0, pagesize = 20) {
  ## Set up to query Open Targets Genetics API

  cli::cli_progress_step("Connecting the database...", spinner = TRUE)
  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()

  variables <- list(studyId = studyid, pageIndex = pageindex, pageSize = pagesize)

  query <- "query topoverlapstudiesquery($studyId: String!, $pageIndex: Int!, $pageSize:Int!){
  topOverlappedStudies(studyId: $studyId, pageIndex: $pageIndex, pageSize: $pageSize) {
    study {
    studyId
    traitReported
    traitCategory
    }
    topStudiesByLociOverlap{
    studyId
    study{
      studyId
      traitReported
      traitCategory
    }
    numOverlapLoci
  }
  }
}"

  ## Execute the query
  otg_qry$query(name = "topoverlapstudies_query", x = query)

  cli::cli_progress_step("Downloading data...", spinner = TRUE)
  top_ov_studies <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$topoverlapstudies_query, variables, flatten = TRUE))$data

  top_ov_studies <- top_ov_studies$topOverlappedStudies %>% as.data.frame()

  return(top_ov_studies)
}
