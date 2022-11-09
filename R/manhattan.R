#' Gives list of manhattan association for a given study.
#'
#' @param studyid is the Open Target Genetics generated id for gwas studies.
#' @param pageIndex optional argument
#' @param pageSize optional argument
#' @export

manhattan <- function(studyid, pageindex=0, pagesize=0) {

  ## Set up to query Open Targets Genetics API

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

  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$manhattan_query, variables), flatten=TRUE)$data

  result_df <- result$manhattan$associations %>% as.data.frame


  return(result_df)

}
