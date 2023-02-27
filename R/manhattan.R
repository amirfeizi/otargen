#' Manhattan association for a given study.
#'
#' @param studyid String: Open Targets Genetics generated id for GWAS study.
#' @param pageIndex Int: Index of the current page, pagination index >= 0.
#' @param pageSize Int: No. of records in a page, pagination size > 0.
#'
#' @return Dataframe containing manhattan associations for the input study id given.
#'
#' @examples
#' manhattan(studyid = "GCST90002357")
#' or
#' manhattan(studyid = "GCST90002357", pageindex = 2, pagesize = 50)
#'
#' @export
#'
#'
manhattan <- function(studyid, pageindex = 0, pagesize = 20) {
  ## Set up to query Open Targets Genetics API

  cli::cli_progress_step("Connecting the database...", spinner = TRUE)
  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()

  variables <- list(studyId = studyid, pageIndex = pageindex, pageSize = pagesize)

  query <- "query manhattanquery($studyId: String!, $pageIndex: Int!, $pageSize:Int!){
  manhattan(studyId:$studyId, pageIndex: $pageIndex, pageSize:$pageSize) {
    associations{
      pvalMantissa
      pvalExponent
      credibleSetSize
      ldSetSize
      totalSetSize
      variant{
        id
        position
        chromosome
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
      bestGenes{
        gene{
          id
          symbol
          chromosome
          start
          end
        }
        score
      }
    bestColocGenes{
      gene{
          id
          symbol
          chromosome
          start
          end
        }
        score
    }
    bestLocus2Genes{
      gene{
          id
          symbol
          chromosome
          start
          end
        }
        score
    }
    }
}
}"


  ## Execute the query
  otg_qry$query(name = "manhattan_query", x = query)

  cli::cli_progress_step("Downloading data...", spinner = TRUE)
  man_result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$manhattan_query, variables), flatten = TRUE)$data
  man_result <- as.data.frame(man_result$manhattan$associations)

  if (nrow(man_result) != 0) {
    man_result <- as.data.frame(tidyr::unnest(man_result, bestGenes, names_sep = ".", keep_empty = TRUE))
    man_result <- as.data.frame(tidyr::unnest(man_result, bestColocGenes, names_sep = ".", keep_empty = TRUE))
    man_result <- as.data.frame(tidyr::unnest(man_result, bestLocus2Genes, names_sep = ".", keep_empty = TRUE))
  }

  return(man_result)
}
