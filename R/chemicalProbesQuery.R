#' Retrieve Chemical Probes data for a specified gene.
#'
#' This function queries the Open Targets Genetics GraphQL API to retrieve chemical probes data
#' for a specified gene.
#'
#' @param ensgId Character: ENSEMBL ID of the target gene (e.g., ENSG00000105397).
#' @param cursor Character: Cursor for pagination (default: NULL).
#' @param size Integer: Number of records to retrieve (default: 10).
#'
#' @return Returns a tibble containing chemical probes data for the specified gene.
#' @examples
#' \dontrun{
#' result <- chemicalProbesQuery(ensgId = "ENSG00000105397", size = 10)
#' result <- chemicalProbesQuery(ensgId = "ENSG00000105397", cursor = NULL, size = 10)
#' }
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble
#' @export
#'

chemicalProbesQuery <- function(ensgId, cursor = NULL, size = 10) {
  if (missing(ensgId) || is.null(ensgId)) {
    stop("Please provide a value for the 'ensgId' argument.")
  }
  
  # Set up to query Open Targets Genetics API
  tryCatch({
    cli::cli_progress_step("Connecting to the Open Targets Genetics GraphQL API...", spinner = TRUE)
    con <- ghql::GraphqlClient$new("https://api.platform.opentargets.org/api/v4/graphql")
    qry <- ghql::Query$new()
    
    # Define base query
    query <- "query ChemicalProbesQuery($ensgId: String!, $size: Int = 10{optional_cursor}) {
      target(ensemblId: $ensgId) {
        id
        chemicalProbes(size: $size{cursor_param}) {
          count
          cursor
          rows {
            id
            control
            drugId
            isHighQuality
            mechanismOfAction
            origin
            probeMinerScore
            probesDrugsScore
            scoreInCells
            scoreInOrganisms
            targetFromSourceId
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
      query <- sub("\\{cursor_param\\}", ", cursor: $cursor", query)
    } else {
      query <- sub("\\{optional_cursor\\}", "", query)
      query <- sub("\\{cursor_param\\}", "", query)
    }
    
    variables <- list(
      ensgId = ensgId,
      size = size
    )
    if (!is.null(cursor)) {
      variables$cursor <- cursor
    }
    
    qry$query(name = "getChemicalProbesData", x = query)
    
    cli::cli_progress_step(paste0("Downloading data for ENSEMBL ID: ", ensgId, " ..."), spinner = TRUE)
    
    # Execute the query
    output0 <- con$exec(qry$queries$getChemicalProbesData, variables)
    output1 <- jsonlite::fromJSON(output0, flatten = TRUE)
    
    if (length(output1$data$target$chemicalProbes$rows) != 0) {
      final_output <- tibble::as_tibble(output1$data$target$chemicalProbes$rows)
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
