#' Retrieve Safety Liabilities data for a specified gene.
#'
#' This function queries the Open Targets GraphQL API to retrieve safety liabilities data
#' for a specified gene.
#'
#' @param ensgId Character: ENSEMBL ID of the target gene (e.g., ENSG00000141510).
#'
#' @return Returns a tibble containing safety liabilities data for the specified gene.
#' @examples
#' \dontrun{
#' result <- safetyQuery(ensgId = "ENSG00000141510")
#' }
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble
#' @export
#'
safetyQuery <- function(ensgId) {
  if (missing(ensgId) || is.null(ensgId)) {
    stop("Please provide a value for the 'ensgId' argument.")
  }
  
  # Set up to query Open Targets API
  tryCatch({
    cli::cli_progress_step("Connecting to the Open Targets GraphQL API...", spinner = TRUE)
    con <- ghql::GraphqlClient$new("https://api.platform.opentargets.org/api/v4/graphql")
    qry <- ghql::Query$new()
    
    query <- "query SafetyQuery($ensgId: String!) {
      target(ensemblId: $ensgId) {
        id
        safetyLiabilities {
          event
          eventId
          biosamples {
            cellFormat
            cellLabel
            tissueLabel
            tissueId
          }
          effects {
            dosing
            direction
          }
          studies {
            name
            type
            description
          }
          datasource
          literature
          url
        }
      }
    }"
    
    variables <- list(
      ensgId = ensgId
    )
    
    qry$query(name = "getSafetyData", x = query)
    
    cli::cli_progress_step(paste0("Downloading data for ENSEMBL ID: ", ensgId, " ..."), spinner = TRUE)
    
    # Execute the query
    output0 <- con$exec(qry$queries$getSafetyData, variables)
    output1 <- jsonlite::fromJSON(output0, flatten = TRUE)
    
    if (length(output1$data$target$safetyLiabilities) != 0) {
      final_output <- tibble::as_tibble(output1$data$target$safetyLiabilities)
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
