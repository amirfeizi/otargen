#' Retrieve Known Drugs data for a specified drug.
#'
#' This function queries the Open Targets GraphQL API to retrieve known drugs data
#' for a specified drug.
#'
#' @param chemblId Character: ChEMBL ID of the target drug (e.g., "CHEMBL1016").
#' @param cursor Character: Cursor for pagination (default: NULL).
#' @param freeTextQuery Character: Free text query to filter results (default: NULL).
#' @param size Integer: Number of records to retrieve (default: 10).
#'
#' @return Returns a tibble containing known drugs data for the specified drug.
#' @examples
#' \dontrun{
#' result <- knownDrugsChemblQuery(chemblId = "CHEMBL1016", size = 10)
#' result <- knownDrugsChemblQuery(chemblId = "CHEMBL1016", cursor = NULL, 
#' freeTextQuery = NULL, size = 10)
#' }
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble
#' @export
#'
knownDrugsChemblQuery <- function(chemblId, cursor = NULL, freeTextQuery = NULL, size = 10) {
  if (missing(chemblId) || is.null(chemblId)) {
    stop("Please provide a value for the 'chemblId' argument.")
  }
  
  # Set up to query Open Targets API
  tryCatch({
    cli::cli_progress_step("Connecting to the Open Targets GraphQL API...", spinner = TRUE)
    con <- ghql::GraphqlClient$new("https://api.platform.opentargets.org/api/v4/graphql")
    qry <- ghql::Query$new()
    
    # knownDrugs removed from Drug type; replaced with indications + clinicalReports
    query <- "query KnownDrugsQuery($chemblId: String!) {
      drug(chemblId: $chemblId) {
        id
        indications {
          count
          rows {
            maxClinicalStage
            disease {
              id
              name
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
      chemblId = chemblId
    )
    
    qry$query(name = "getKnownDrugsData", x = query)
    
    cli::cli_progress_step(paste0("Downloading data for ChEMBL ID: ", chemblId, " ..."), spinner = TRUE)
    
    # Execute the query
    output0 <- con$exec(qry$queries$getKnownDrugsData, variables)
    output1 <- jsonlite::fromJSON(output0, flatten = TRUE)
    
    if (length(output1$data$drug$indications$rows) != 0) {
      final_output <- tibble::as_tibble(output1$data$drug$indications$rows) %>%
        dplyr::mutate(
          drugId = output1$data$drug$id,
          indicationsCount = output1$data$drug$indications$count
        )
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
