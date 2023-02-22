#' Retrieves the top studies having overlap with the given input study.
#'
#' For an input study id, a table is generated with the following columns -
#' input studyid (study.studyId), study.traitReported, study.traitCategory,
#' top study ids ordered by loci overlap (topStudiesByLociOverlap.studyId),
#' topStudiesByLociOverlap.study.traitReported, topStudiesByLociOverlap.study.traitCategory
#' and number of overlap with the referenced study (topStudiesByLociOverlap.numOverlapLoci).
#'
#'
#' @param studyid String: Open Target Genetics generated id for GWAS study.
#' @param pageIndex Int: Index of the current page, pagination index >= 0.
#' @param pageSize Int: No. of records in a page, pagination size > 0.
#'
#' @returns Data frame with top studies containing the above mentioned columns.
#'
#' @examples
#'
#' topOverlappedStudies(studyid="GCST006614_3")
#' or
#' topOverlappedStudies(studyid="NEALE2_6177_1", pageindex=1, pagesize=50)
#'
#' @export
#'
#'
#'
topOverlappedStudies <- function(studyid, pageindex=0, pagesize=20) {

  ## Set up to query Open Targets Genetics API

  cli::cli_progress_step("Connecting the database...", spinner = TRUE )
  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()

  variables <- list(studyId = studyid, pageIndex = pageindex, pageSize=pagesize)

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
  otg_qry$query(name = "topoverlapstudies_query", x = query )

  cli::cli_progress_step("Downloading data...", spinner = TRUE)
  top_ov_studies <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$topoverlapstudies_query, variables, flatten=TRUE))$data

  top_ov_studies <- top_ov_studies$topOverlappedStudies %>% as.data.frame

  return(top_ov_studies)
}
