#' Retrieve Variants data for a specified study locus.
#'
#' This function queries the Open Targets GraphQL API to retrieve variants data
#' for a specified study locus.
#'
#' @param studyLocusId Character: ID of the target study locus (e.g., "fa375739ca2a6b825ce5cc69d117e84b").
#' @param size Integer: Number of records to retrieve (default: 500).
#' @param index Integer: Page index for pagination (default: 0).
#'
#' @return Returns a tibble containing variants data for the specified study locus.
#' @examples
#' \dontrun{
#' result <- variantsQuery(studyLocusId = "fa375739ca2a6b825ce5cc69d117e84b", 
#' size = 500, index = 0)
#' }
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble
#' @export
#'
variantsQuery <- function(studyLocusId, size = 500, index = 0) {
  if (missing(studyLocusId) || is.null(studyLocusId)) {
    stop("Please provide a value for the 'studyLocusId' argument.")
  }
  
  # Set up to query Open Targets API
  tryCatch({
    cli::cli_progress_step("Connecting to the Open Targets GraphQL API...", spinner = TRUE)
    con <- ghql::GraphqlClient$new("https://api.platform.opentargets.org/api/v4/graphql")
    qry <- ghql::Query$new()
    
    query <- "query VariantsQuery($studyLocusId: String!, $size: Int!, $index: Int!) {
      credibleSet(studyLocusId: $studyLocusId) {
        studyLocusId
        locus(page: { size: $size, index: $index }) {
          count
          rows {
            logBF
            posteriorProbability
            variant {
              id
              chromosome
              position
              referenceAllele
              alternateAllele
              mostSevereConsequence {
                id
                label
              }
            }
            pValueMantissa
            pValueExponent
            beta
            standardError
            r2Overall
          }
        }
      }
    }"
    
    variables <- list(
      studyLocusId = studyLocusId,
      size = size,
      index = index
    )
    
    qry$query(name = "getVariantsData", x = query)
    
    cli::cli_progress_step(paste0("Downloading data for study locus ID: ", studyLocusId, " ..."), spinner = TRUE)
    
    # Execute the query
    output0 <- con$exec(qry$queries$getVariantsData, variables)
    output1 <- jsonlite::fromJSON(output0, flatten = TRUE)
    
    if (length(output1$data$credibleSet$locus$rows) != 0) {
      final_output <- tibble::as_tibble(output1$data$credibleSet$locus$rows) %>%
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
