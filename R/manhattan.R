#' Manhattan association for a given study.
#'
#' @param studyid is the Open Target Genetics generated id for gwas studies.
#' @param pageIndex pageIndex pagination index >= 0. Index of the current page.
#' @param pageSize pagination size > 0. No. of records in a page. Default: 20
#' @return A dataframe containing manhattan associations for the input study id given.
#' @examples
#' manhattan("GCST90002357")
#' manhattan("GCST90002357", 2, 50)
#' @export
#'
#'

manhattan <- function(studyid, pageindex=0, pagesize=20) {

  ## Set up to query Open Targets Genetics API

  cli::cli_progress_step("Connecting the database...", spinner = TRUE)
  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()

  variables <- list(studyId = studyid, pageIndex = pageindex, pageSize=pagesize)

  query <- "query manhattanquery($studyId: String!, $pageIndex: Int!, $pageSize:Int!){
  manhattan(studyId: $studyId, pageIndex: $pageIndex, pageSize: $pageSize) {
    associations{
      pvalMantissa
      pvalExponent
      credibleSetSize
      ldSetSize
      totalSetSize
      variant{
        id
        position
        rsId
      }
      pval
      oddsRatio
      oddsRatioCILower
      oddsRatioCIUpper
      beta
      betaCILower
      betaCIUpper
      direction
    }
  }
}"


  ## Execute the query
  otg_qry$query(name = "manhattan_query", x = query)

  cli::cli_progress_step("Downloading data...", spinner = TRUE)
  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$manhattan_query, variables), flatten=TRUE)$data

  result_df <- result$manhattan$associations %>% as.data.frame


  return(result_df)

}
