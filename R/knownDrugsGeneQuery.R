#' Retrieve drug and clinical candidate data for a specified gene.
#'
#' This function queries the Open Targets GraphQL API to retrieve drug
#' and clinical candidate data for a specified gene.
#'
#' @param ensgId Character: ENSEMBL ID of the target gene (e.g., ENSG00000169174).
#'
#' @return Returns a data frame containing drug and clinical candidate data for the specified gene.
#' @examples
#' \dontrun{
#' result <- knownDrugsGeneQuery(ensgId = "ENSG00000169174")
#' }
#' @importFrom magrittr %>%
#' @export
#'
knownDrugsGeneQuery <- function(ensgId) {
  if (missing(ensgId) || is.null(ensgId)) {
    stop("Please provide a value for the 'ensgId' argument.")
  }

  # Set up to query Open Targets API
  tryCatch({
    cli::cli_progress_step("Connecting to the Open Targets GraphQL API...", spinner = TRUE)
    con <- ghql::GraphqlClient$new("https://api.platform.opentargets.org/api/v4/graphql")
    qry <- ghql::Query$new()

    # knownDrugs removed from Target type; replaced with drugAndClinicalCandidates
    query <- "query KnownDrugsQuery($ensgId: String!) {
      target(ensemblId: $ensgId) {
        id
        drugAndClinicalCandidates {
          count
          rows {
            maxClinicalStage
            drug {
              id
              name
              drugType
              mechanismsOfAction {
                rows {
                  actionType
                  targets {
                    id
                  }
                }
              }
            }
            diseases {
              diseaseFromSource
              disease {
                id
                name
              }
            }
            clinicalReports {
              id
              source
              url
              clinicalStage
              trialOverallStatus
            }
          }
        }
      }
    }"

    variables <- list(
      ensgId = ensgId
    )

    qry$query(name = "getKnownDrugsData", x = query)

    cli::cli_progress_step(paste0("Downloading data for ENSEMBL ID: ", ensgId, " ..."), spinner = TRUE)

    # Execute the query
    output0 <- con$exec(qry$queries$getKnownDrugsData, variables)
    output1 <- jsonlite::fromJSON(output0, flatten = TRUE)

    if (length(output1$data$target$drugAndClinicalCandidates$rows) != 0) {
      final_output <- output1$data$target$drugAndClinicalCandidates$rows
      return(final_output)
    } else {
      message("No data found for the given parameters.")
      return(NULL)
    }

  }, error = function(e) {
    # Handling connection timeout
    if (grepl("Timeout was reached", e$message)) {
      stop("Connection timeout reached while connecting to the Open Targets GraphQL API.")
    } else {
      stop(e) # Handle other types of errors
    }
  })
}
