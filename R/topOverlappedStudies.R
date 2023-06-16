#' Retrieves the top studies having overlap with the given input study.
#'
#' Fetches the top associated studies for a given study ID ordered by loci overlap.
#'
#' @param \emph{studyid} String: Open Targets Genetics generated ID for a GWAS study.
#' @param \emph{pageindex} Integer: Index of the current page for pagination (>= 0).
#' @param \emph{pagesize} Integer: Number of records in a page for pagination (> 0).
#'
#' @return Returns a data frame with the top studies containing the following columns:
#' \itemize{
#'   \item{\code{study.studyId}:} \emph{Character}. Study ID of the input study.
#'   \item{\code{study.traitReported}:} \emph{Character}. Reported trait of the input study.
#'   \item{\code{study.traitCategory}:} \emph{Character}. Category of the trait in the input study.
#'   \item{\code{topStudiesByLociOverlap.studyId}:} \emph{Character}. Study ID of the top associated studies.
#'   \item{\code{topStudiesByLociOverlap.study.studyId}:} \emph{Character}. Study ID of the top associated studies.
#'   \item{\code{topStudiesByLociOverlap.study.traitReported}:} \emph{Character}. Reported trait of the top associated studies.
#'   \item{\code{topStudiesByLociOverlap.study.traitCategory}:} \emph{Character}. Category of the trait in the top associated studies.
#'   \item{\code{topStudiesByLociOverlap.numOverlapLoci}:} \emph{Integer}. Number of loci overlapped with the input study.
#' }
#'
#' @examples
#' \dontrun{
#' result <- topOverlappedStudies(studyid = "GCST006614_3")
#' result <- topOverlappedStudies(studyid = "NEALE2_6177_1", pageindex = 1, pagesize = 50)
#'}
#' @importFrom magrittr %>%
#' @export
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
