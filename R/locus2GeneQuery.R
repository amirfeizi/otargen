#' Retrieve Locus-to-Gene Predictions data for a specified study locus.
#'
#' This function queries the Open Targets GraphQL API to retrieve locus-to-gene prediction data
#' for a specified study locus.
#'
#' @param studyLocusId Character: ID of the target study locus (e.g., "fa375739ca2a6b825ce5cc69d117e84b").
#'
#' @return Returns a tibble containing locus-to-gene prediction data for the specified study locus.
#' @examples
#' \dontrun{
#' result <- locus2GeneQuery(studyLocusId = "fa375739ca2a6b825ce5cc69d117e84b")
#' }
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble
#' @export
#'
locus2GeneQuery <- function(studyLocusId) {
  if (missing(studyLocusId) || is.null(studyLocusId)) {
    stop("Please provide a value for the 'studyLocusId' argument.")
  }
  
  # Set up to query Open Targets API
  tryCatch({
    cli::cli_progress_step("Connecting to the Open Targets GraphQL API...", spinner = TRUE)
    con <- ghql::GraphqlClient$new("https://api.platform.opentargets.org/api/v4/graphql")
    qry <- ghql::Query$new()
    
    query <- "query Locus2GeneQuery($studyLocusId: String!) {
      credibleSet(studyLocusId: $studyLocusId) {
        l2GPredictions {
          count
          rows {
            shapBaseValue
            features {
              shapValue
              value
              name
            }
            score
            target {
              id
              approvedSymbol
            }
          }
        }
      }
    }"
    
    variables <- list(
      studyLocusId = studyLocusId
    )
    
    qry$query(name = "getLocus2GeneData", x = query)
    
    cli::cli_progress_step(paste0("Downloading data for study locus ID: ", studyLocusId, " ..."), spinner = TRUE)
    
    # Execute the query
    output0 <- con$exec(qry$queries$getLocus2GeneData, variables)
    output1 <- jsonlite::fromJSON(output0, flatten = TRUE)
    
    if (length(output1$data$credibleSet$l2GPredictions$rows) != 0) {
      final_output <- tibble::as_tibble(output1$data$credibleSet$l2GPredictions$rows) %>%
        dplyr::mutate(
          studyLocusId = output1$data$credibleSet$studyLocusId
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
