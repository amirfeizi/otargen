#' Retrieve ChEMBL data for a specified gene and disease.
#'
#' This function queries the Open Targets Genetics GraphQL API to retrieve ChEMBL data
#' for a specified gene and disease, including evidence from the ChEMBL datasource.
#'
#' @param ensemblId Character: ENSEMBL ID of the target gene (e.g., ENSG00000169174).
#' @param efoId Character: EFO ID of the disease (e.g., EFO_0004911).
#' @param size Integer: Number of records to retrieve (default: 10).
#' @param cursor Character: Cursor for pagination (default: NULL).
#'
#' @return Returns a data frame containing ChEMBL data for the specified gene and disease.
#' @examples
#' \dontrun{
#' result <- chemblQuery(ensemblId = "ENSG00000169174",
#'  efoId = "EFO_0004911", size = 10)
#' result <- chemblQuery(ensemblId = "ENSG00000169174",
#'  efoId = "EFO_0004911", size = 10, cursor = NULL)
#' }
#' @importFrom magrittr %>%
#' @export
#'
chemblQuery <- function(ensemblId, efoId, size = 10, cursor = NULL) {
  if (missing(ensemblId) || is.null(ensemblId)) {
    stop("Please provide a value for the 'ensemblId' argument.")
  }
  if (missing(efoId) || is.null(efoId)) {
    stop("Please provide a value for the 'efoId' argument.")
  }

  # Set up to query Open Targets Genetics API
  tryCatch({
    cli::cli_progress_step("Connecting to the Open Targets Genetics GraphQL API...", spinner = TRUE)
    con <- ghql::GraphqlClient$new("https://api.platform.opentargets.org/api/v4/graphql")
    qry <- ghql::Query$new()

    query <- "query ChemblQuery($ensemblId: String!, $efoId: String!, $size: Int!) {
      disease(efoId: $efoId) {
        id
        chembl: evidences(
          ensemblIds: [$ensemblId]
          enableIndirect: true
          datasourceIds: [\"chembl\"]
          size: $size
        ) {
          count
          rows {
            disease {
              id
              name
            }
            target {
              id
              approvedSymbol
            }
            drug {
              id
              name
              drugType
              mechanismsOfAction {
                rows {
                  mechanismOfAction
                  targets {
                    id
                    approvedSymbol
                  }
                }
              }
            }
            variantEffect
            directionOnTrait
            targetFromSourceId
            clinicalPhase
            clinicalStatus
            studyStartDate
            studyStopReason
            studyStopReasonCategories
            urls {
              niceName
              url
            }
          }
        }
      }
    }"

    variables <- list(
      ensemblId = ensemblId,
      efoId = efoId,
      size = size,
      cursor = cursor
    )

    qry$query(name = "getChemblData", x = query)

    cli::cli_progress_step(paste0("Downloading data for ENSEMBL ID: ", ensemblId, " and EFO ID: ", efoId, " ..."), spinner = TRUE)

    # Execute the query
    output0 <- con$exec(qry$queries$getChemblData, variables)
    output1 <- jsonlite::fromJSON(output0, flatten = TRUE)

    if (length(output1$data$disease$chembl$rows) != 0) {
      final_output <- output1$data$disease$chembl$rows
      return(final_output)
    } else {
      message("No data found for the given parameters.")
      return(NULL)
    }

  }, error = function(e) {
    # Handling connection timeout
    if (grepl("Timeout was reached", e$message)) {
      stop("Connection timeout reached while connecting to the Open Targets Genetics GraphQL API.")
    } else {
      stop(e) # Handle other types of errors
    }
  })
}
