#' Retrieve Indications data for a specified drug.
#'
#' This function queries the Open Targets GraphQL API to retrieve indications data
#' for a specified drug.
#'
#' @param chemblId Character: ChEMBL ID of the target drug (e.g., "CHEMBL1016").
#'
#' @return Returns a tibble containing indications data for the specified drug.
#' @examples
#' \dontrun{
#' result <- indicationsQuery(chemblId = "CHEMBL1016")
#' }
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble
#' @export
#'
indicationsQuery <- function(chemblId) {
  if (missing(chemblId) || is.null(chemblId)) {
    stop("Please provide a value for the 'chemblId' argument.")
  }
  
  # Set up to query Open Targets API
  tryCatch({
    cli::cli_progress_step("Connecting to the Open Targets GraphQL API...", spinner = TRUE)
    con <- ghql::GraphqlClient$new("https://api.platform.opentargets.org/api/v4/graphql")
    qry <- ghql::Query$new()
    
    query <- "query IndicationsQuery($chemblId: String!) {
      drug(chemblId: $chemblId) {
        id
        indications {
          rows {
            maxPhaseForIndication
            disease {
              id
              name
              therapeuticAreas {
                id
                name
              }
            }
            references {
              ids
              source
            }
          }
          count
        }
      }
    }"
    
    variables <- list(
      chemblId = chemblId
    )
    
    qry$query(name = "getIndicationsData", x = query)
    
    cli::cli_progress_step(paste0("Downloading data for ChEMBL ID: ", chemblId, " ..."), spinner = TRUE)
    
    # Execute the query
    output0 <- con$exec(qry$queries$getIndicationsData, variables)
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
