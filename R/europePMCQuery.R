#' Retrieve Europe PMC data for a specified gene and disease.
#'
#' This function queries the Open Targets GraphQL API to retrieve Europe PMC evidence data
#' for a specified gene and disease.
#'
#' @param ensemblId Character: ENSEMBL ID of the target gene (e.g., "ENSG00000080815").
#' @param efoId Character: EFO ID of the target disease (e.g., "MONDO_0004975").
#' @param cursor Character: Cursor for pagination (default: NULL).
#' @param size Integer: Number of records to retrieve (default: 50).
#'
#' @return Returns a tibble containing Europe PMC evidence data for the specified gene and disease.
#' @examples
#' \dontrun{
#' result <- europePMCQuery(ensemblId = "ENSG00000080815",
#'  efoId = "MONDO_0004975", size = 50)
#' result <- europePMCQuery(ensemblId = "ENSG00000080815", 
#' efoId = "MONDO_0004975", cursor = NULL, size = 50)
#' }
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble
#' @export
#'
europePMCQuery <- function(ensemblId, efoId, cursor = NULL, size = 50) {
  if (missing(ensemblId) || is.null(ensemblId)) {
    stop("Please provide a value for the 'ensemblId' argument.")
  }
  if (missing(efoId) || is.null(efoId)) {
    stop("Please provide a value for the 'efoId' argument.")
  }
  
  # Set up to query Open Targets API
  tryCatch({
    cli::cli_progress_step("Connecting to the Open Targets GraphQL API...", spinner = TRUE)
    con <- ghql::GraphqlClient$new("https://api.platform.opentargets.org/api/v4/graphql")
    qry <- ghql::Query$new()
    
    # Define base query with placeholders
    query <- "query EuropePMCQuery($ensemblId: String!, $efoId: String!, $size: Int!{optional_cursor}) {
      disease(efoId: $efoId) {
        id
        europePmc: evidences(
          ensemblIds: [$ensemblId]
          enableIndirect: true
          size: $size
          datasourceIds: [\"europepmc\"]
          {cursor_param}
        ) {
          count
          cursor
          rows {
            disease {
              name
              id
            }
            target {
              approvedSymbol
              id
            }
            literature
            textMiningSentences {
              tStart
              tEnd
              dStart
              dEnd
              section
              text
            }
            resourceScore
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
    
    qry$query(name = "getEuropePMCData", x = query)
    
    cli::cli_progress_step(paste0("Downloading data for ENSEMBL ID: ", ensemblId, " and EFO ID: ", efoId, " ..."), spinner = TRUE)
    
    # Execute the query
    output0 <- con$exec(qry$queries$getEuropePMCData, variables)
    output1 <- jsonlite::fromJSON(output0, flatten = TRUE)
    
    if (length(output1$data$disease$europePmc$rows) != 0) {
      final_output <- tibble::as_tibble(output1$data$disease$europePmc$rows) %>%
        dplyr::mutate(
          diseaseId = output1$data$disease$id,
          europePmcCount = output1$data$disease$europePmc$count,
          cursor = output1$data$disease$europePmc$cursor
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
