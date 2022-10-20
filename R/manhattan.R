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
  otg_qry$query("manhattan_query", "query manhattanquery($studyId: String!, $pageIndex: Int!, $pageSize:Int!){
  manhattan(studyId: $studyId, pageIndex: $pageIndex, pageSize: $pageSize) {
    associations{
      pvalMantissa
      pvalExponent
      credibleSetSize
      ldSetSize
      totalSetSize
      variant{
        id
      }
      pval
      oddsRatio
      beta
      direction
      bestGenes{
        gene{
          id
          bioType
        }
        score
      }
      bestColocGenes{
        gene{
          id
          bioType
        }
        score
      }
      bestLocus2Genes{
        gene{
          id
          bioType
        }
        score
      }

    }
  }
}")

  ## Execute the query
  variables <- list(studyId = studyid, pageIndex = pageindex, pageSize=pagesize)

  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$manhattan_query, variables, flatten=TRUE))$data

  result_df <- result %>% as.data.frame

  return(result_df)
}
