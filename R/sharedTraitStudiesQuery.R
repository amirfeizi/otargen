#' Retrieve Shared Trait Studies data for specified diseases.
#'
#' This function queries the Open Targets GraphQL API to retrieve shared trait studies data
#' for specified disease IDs.
#'
#' @param diseaseIds Character vector: IDs of the target diseases (e.g., c("EFO_0004587")).
#' @param size Integer: Number of records to retrieve (default: 500).
#' @param index Integer: Page index for pagination (default: 0).
#'
#' @return Returns a tibble containing shared trait studies data for the specified diseases.
#' @examples
#' \dontrun{
#' result <- sharedTraitStudiesQuery(diseaseIds = c("EFO_0004587"), size = 500, 
#' index = 0)
#' }
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble
#' @export
#'
sharedTraitStudiesQuery <- function(diseaseIds, size = 500, index = 0) {
  if (missing(diseaseIds) || is.null(diseaseIds) || length(diseaseIds) == 0) {
    stop("Please provide a value for the 'diseaseIds' argument.")
  }
  
  # Set up to query Open Targets API
  tryCatch({
    cli::cli_progress_step("Connecting to the Open Targets GraphQL API...", spinner = TRUE)
    con <- ghql::GraphqlClient$new("https://api.platform.opentargets.org/api/v4/graphql")
    qry <- ghql::Query$new()
    
    query <- "query SharedTraitStudiesQuery($diseaseIds: [String!]!, $size: Int!, $index: Int!) {
      studies(diseaseIds: $diseaseIds, page: { size: $size, index: $index }) {
        count
        rows {
          id
          traitFromSource
          projectId
          diseases {
            id
            name
          }
          publicationFirstAuthor
          publicationDate
          publicationJournal
          nSamples
          cohorts
          ldPopulationStructure {
            ldPopulation
            relativeSampleSize
          }
          pubmedId
        }
      }
    }"
    
    variables <- list(
      diseaseIds = diseaseIds,
      size = size,
      index = index
    )
    
    qry$query(name = "getSharedTraitStudiesData", x = query)
    
    cli::cli_progress_step(paste0("Downloading data for disease IDs: ", paste(diseaseIds, collapse = ", "), " ..."), spinner = TRUE)
    
    # Execute the query
    output0 <- con$exec(qry$queries$getSharedTraitStudiesData, variables)
    output1 <- jsonlite::fromJSON(output0, flatten = TRUE)
    
    if (length(output1$data$studies$rows) != 0) {
      final_output <- tibble::as_tibble(output1$data$studies$rows)
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
