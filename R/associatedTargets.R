#' Get associated targets for a disease category
#'
#' Retrive specified number of drug targets (n) associated with a disease with specified EFO id
#'
#' @param efoid A string. The Experimental Factor Ontology(EFO) id
#' @param n  An integer. Number of the record to return
#' @return Returns a dataframe including a data table of the associated drug targets
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
  #variables <- list(efoId = )

  result <- jsonlite::fromJSON(otp_cli$exec(otp_qry$queries$simple_query, variables, flatten = TRUE))$data$disease

  # list of data frames with data source id and scores in long format
  data <- result$associatedTargets$rows$datatypeScores

  # converting each data frames to wide format
  score_dt <-   lapply(data,tidyr::spread, key = id, value = score)

  # merging all data frames with filling missing data
  score_dt_wd <- data.table::rbindlist(score_dt, fill = TRUE)

  # adding the target gene information
  results_final <- cbind(result$associatedTargets$rows$target, score_dt_wd) %>%
    dplyr::rename(ensembl_id = .data$id)

  return(results_final[1:n, ])
}
