#' Collect litreture evidance for known drug for a disease
#'
#' @param efoid is experimental factor ontology
#' @return a dataframe including the queried gene indentity and its colocalization data
#' @export


literatureDrugQuery <- function(efoid) {
  otp_cli <- ghql::GraphqlClient$new(url = "https://api.platform.opentargets.org/api/v4/graphql")
  otp_qry <- ghql::Query$new()

  ## Query for targets associated with a disease
  otp_qry$query("litreture_query", 'query literatureDrugQuery($efoId: String!){
  disease(efoId: $efoId) {
    name
    associatedTargets(
      aggregationFilters: [{ name: "dataTypes", path: "literature" }]
    ) {
      rows {
        score
        target {
          id
          approvedSymbol
          evidences (
            efoIds: [$efoId]
            datasourceIds: ["europepmc"]
          ){
            count
            rows {
              textMiningSentences {
                text
              }
            }
          }
          knownDrugs {
            uniqueDrugs
            rows {
              drug {
                id
                name
                maximumClinicalTrialPhase
              }
            }
          }
        }
      }
    }
  }
}')


  ## Execute the query
  variables <- list(efoId = "EFO_0000540")
  result <- jsonlite::fromJSON(otp_cli$exec(otp_qry$queries$litreture_query,
                                            variables, flatten = TRUE))$data$disease

  result_targets <- result$associatedTargets$rows$target[, 1:2]
  result_targets$score <- result$associatedTargets$rows$score

  result_targets$text_mining <- result$associatedTargets$rows$target$evidences$rows

  result_targets <-  tidyr::unnest(cols = c(result_targets$text_mining))
  result_targets <- tidyr::unnest(cols = c(result_targets$textMiningSentences))

  result_targets <- dplyr::distinct(result_targets)

  result_drugs <- result$associatedTargets$rows$target[, 1:2]

  result_drugs$drugs <- result$associatedTargets$rows$target$knownDrugs$rows
  result_drugs <-  tidyr::unnest(cols = c(result_drugs$drugs))

  return(result_targets, result_drugs)
}
