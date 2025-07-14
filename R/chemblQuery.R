#' Retrieve ChEMBL data for a specified gene and disease.
#'
#' This function queries the Open Targets GraphQL API to retrieve ChEMBL evidence data
#' for a specified gene and disease.
#'
#' @param ensemblId Character: ENSEMBL ID of the target gene (e.g., "ENSG00000080815").
#' @param efoId Character: EFO ID of the target disease (e.g., "MONDO_0004975").
#' @param cursor Character: Cursor for pagination (default: NULL).
#' @param size Integer: Number of records to retrieve (default: 10).
#'
#' @return Returns a tibble containing ChEMBL evidence data for the specified gene and disease.
#' @examples
#' \dontrun{
#' result <- chemblQuery(ensemblId = "ENSG00000080815", efoId = 
#' "MONDO_0004975",
#'  size = 10)
#' result <- chemblQuery(ensemblId = "ENSG00000080815", efoId = 
#' "MONDO_0004975",
#'  cursor = NULL, size = 10)
#' }
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble
#' @export
#'
chemblQuery <- function(ensemblId, efoId, cursor = NULL, size = 10) {
  if (missing(ensemblId) || is.null(ensemblId)) {
    stop("Please provide a value for the 'ensemblId' argument.")
  }
  if (missing(efoId) || is.null(efoId)) {
    stop("Please provide a value for the 'efoId' argument.")
  }
  
  # Set up to query Open Targets  API
  tryCatch({
    cli::cli_progress_step("Connecting to the Open Targets GraphQL API...", spinner = TRUE)
    con <- ghql::GraphqlClient$new("https://api.platform.opentargets.org/api/v4/graphql")
    qry <- ghql::Query$new()
    
    # Define base query with placeholders
    query <- "query ChemblQuery($ensemblId: String!, $efoId: String!, $size: Int!{optional_cursor}) {
      disease(efoId: $efoId) {
        id
        chembl: evidences(
          ensemblIds: [$ensemblId]
          enableIndirect: true
          datasourceIds: [\"chembl\"]
          size: $size
          {cursor_param}
        ) {
          cursor
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
            cohortPhenotypes
            urls {
              niceName
              url
            }
          }
        }
      }
    }"
    
    # Conditionally include cursor in query and variables
    if (!is.null(cursor)) {
      query <- sub("\\{optional_cursor\\}", ", $cursor: String", query)
      query <- sub("\\{cursor_param\\}", "cursor: $cursor", query)
    } else {
      query <- sub("\\{optional_cursor\\}", "", query)
      query <- sub("\\{cursor_param\\}", "", query)
    }
    
    variables <- list(
      ensemblId = ensemblId,
      efoId = efoId,
      size = size
    )
    if (!is.null(cursor)) {
      variables$cursor <- cursor
    }
    
    qry$query(name = "getChemblData", x = query)
    
    cli::cli_progress_step(paste0("Downloading data for ENSEMBL ID: ", ensemblId, " and EFO ID: ", efoId, " ..."), spinner = TRUE)
    
    # Execute the query
    output0 <- con$exec(qry$queries$getChemblData, variables)
    output1 <- jsonlite::fromJSON(output0, flatten = TRUE)
    
    if (length(output1$data$disease$chembl$rows) != 0) {
      final_output <- tibble::as_tibble(output1$data$disease$chembl$rows) %>%
        dplyr::mutate(
          diseaseId = output1$data$disease$id,
          chemblCount = output1$data$disease$chembl$count,
          cursor = output1$data$disease$chembl$cursor
        )
      return(final_output)
    } else {
      message("No data found for the given parameters.")
      return(NULL)
    }
    
  }, error = function(e) {
    # Handle connection timeout
    if (grepl("Timeout was reached", e$message)) {
      stop("Connection timeout reached while connecting to the Open Targets GraphQL API.")
    } else {
      stop(e) # Handle other types of errors
    }
  })
}
