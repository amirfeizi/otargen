#' Collect litreture evidance for known drug for a disease
#'
#'
#' @param ensembl id is a identification id for genes by ensembl database
#' @return a dataframe including the queried gene indentity and its colocalization
#' data
#' @export


literatureDrugQuery <- function(efoid){

  otp_cli <- GraphqlClient$new(url = 'https://api.platform.opentargets.org/api/v4/graphql')
  otp_qry <- Query$new()

  ## Query for targets associated with a disease
  otp_qry$query('litreture_query', 'query literatureDrugQuery($efoId: String!){
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
}'
  )


  ## Execute the query
  variables <- list(efoId = "EFO_0000540")
  result <- fromJSON(otp_cli$exec(otp_qry$queries$litreture_query, variables, flatten = TRUE))$data$disease

  result_targets <- result$associatedTargets$rows$target[,1:2]
  result_targets$score <- result$associatedTargets$rows$score

  result_targets$text_mining <- result$associatedTargets$rows$target$evidences$rows

  result_targets <- result_targets %>% unnest(cols = c(text_mining)) %>%
    unnest(cols = c(textMiningSentences))

  result_targets <- distinct(result_targets)

  result_drugs <- result$associatedTargets$rows$target[,1:2]

  result_drugs$drugs <- result$associatedTargets$rows$target$knownDrugs$rows
  result_drugs <- result_drugs %>% unnest(cols = c(drugs))

  return(result_targets, result_drugs)

}

