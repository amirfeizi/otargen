#' Get associated targets for a disease category
#'
#' Retrive n drug targets associated with disease with specified EFO id
#'
#' @param efoid A string. The Experimental Factor Ontology-EFO id
#' @param n  An integer. Number of the record to return
#' @return Returns a dataframe which includes data table of the associated drug targets
#' @export
associatedTargets <- function(efoid, n) {
  otp_cli <- ghql::GraphqlClient$new(url = "https://api.platform.opentargets.org/api/v4/graphql")
  otp_qry <- ghql::Query$new()

  ## Query for targets associated with a disease
  otp_qry$query("simple_query", "query simpleQuery($efoId: String!){
  disease(efoId: $efoId){
    name
    associatedTargets{
      rows{
        target{
          id
          approvedName
        }
        datatypeScores{
          id
          score
        }
      }
    }
  }
}")

  ## Execute the query
  variables <- list(efoId = efoid)
  result <- jsonlite::fromJSON(otp_cli$exec(otp_qry$queries$simple_query, variables, flatten = TRUE))$data$disease

  data <- result$associatedTargets$rows$datatypeScores
  score_dt <-   lapply(tidyr::spread, key = data$id, value = data$score)


  score_dt_wd <- data.table::rbindlist(score_dt, fill = TRUE)


  results_final <- cbind(result$associatedTargets$rows$target, score_dt_wd)
  return(results_final[1:n, ])
}
