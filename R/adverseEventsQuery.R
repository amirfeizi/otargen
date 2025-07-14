#' Retrieve Adverse Events data for a specified drug.
#'
#' This function queries the Open Targets GraphQL API to retrieve adverse events data
#' for a specified drug.
#'
#' @param chemblId Character: ChEMBL ID of the target drug (e.g., "CHEMBL1016").
#' @param index Integer: Page index for pagination (default: 0).
#' @param size Integer: Number of records to retrieve (default: 10).
#'
#' @return Returns a tibble containing adverse events data for the specified drug.
#' @examples
#' \dontrun{
#' result <- adverseEventsQuery(chemblId = "CHEMBL1016", size = 10)
#' result <- adverseEventsQuery(chemblId = "CHEMBL1016", index = 0, size = 10)
#' }
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble
#' @export
#'
adverseEventsQuery <- function(chemblId, index = 0, size = 10) {
  if (missing(chemblId) || is.null(chemblId)) {
    stop("Please provide a value for the 'chemblId' argument.")
  }
  
  # Set up to query Open Targets  API
  tryCatch({
    cli::cli_progress_step("Connecting to the Open Targets  GraphQL API...", spinner = TRUE)
    con <- ghql::GraphqlClient$new("https://api.platform.opentargets.org/api/v4/graphql")
    qry <- ghql::Query$new()
    
    query <- "query AdverseEventsQuery($chemblId: String!, $index: Int = 0, $size: Int = 10) {
      drug(chemblId: $chemblId) {
        id
        maxLlr: adverseEvents(page: { index: 0, size: 1 }) {
          rows {
            logLR
          }
        }
        adverseEvents(page: { index: $index, size: $size }) {
          criticalValue
          count
          rows {
            name
            count
            logLR
            meddraCode
          }
        }
      }
    }"
    
    variables <- list(
      chemblId = chemblId,
      index = index,
      size = size
    )
    
    qry$query(name = "getAdverseEventsData", x = query)
    
    cli::cli_progress_step(paste0("Downloading data for ChEMBL ID: ", chemblId, " ..."), spinner = TRUE)
    
    # Execute the query
    output0 <- con$exec(qry$queries$getAdverseEventsData, variables)
    output1 <- jsonlite::fromJSON(output0, flatten = TRUE)
    
    if (length(output1$data$drug$adverseEvents$rows) != 0) {
      final_output <- tibble::as_tibble(output1$data$drug$adverseEvents$rows) %>%
        dplyr::mutate(
          drugId = output1$data$drug$id,
          criticalValue = output1$data$drug$adverseEvents$criticalValue,
          adverseEventsCount = output1$data$drug$adverseEvents$count,
          maxLogLR = output1$data$drug$maxLlr$rows$logLR
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
