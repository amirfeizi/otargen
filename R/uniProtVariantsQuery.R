#' Retrieve UniProt Variants data for a specified variant.
#'
#' This function queries the Open Targets GraphQL API to retrieve UniProt variants data
#' for a specified variant.
#'
#' @param variantId Character: ID of the target variant (e.g., "4_1804392_G_A").
#'
#' @return Returns a tibble containing UniProt variants data for the specified variant.
#' @examples
#' \dontrun{
#' result <- uniProtVariantsQuery(variantId = "4_1804392_G_A")
#' }
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble
#' @export
#'
uniProtVariantsQuery <- function(variantId) {
  if (missing(variantId) || is.null(variantId)) {
    stop("Please provide a value for the 'variantId' argument.")
  }
  
  # Set up to query Open Targets API
  tryCatch({
    cli::cli_progress_step("Connecting to the Open Targets GraphQL API...", spinner = TRUE)
    con <- ghql::GraphqlClient$new("https://api.platform.opentargets.org/api/v4/graphql")
    qry <- ghql::Query$new()
    
    query <- "query UniProtVariantsQuery($variantId: String!) {
      variant(variantId: $variantId) {
        id
        referenceAllele
        alternateAllele
        evidences(datasourceIds: [\"uniprot_variants\"]) {
          count
          rows {
            targetFromSourceId
            confidence
            diseaseFromSource
            disease {
              id
              name
            }
            literature
          }
        }
      }
    }"
    
    variables <- list(
      variantId = variantId
    )
    
    qry$query(name = "getUniProtVariantsData", x = query)
    
    cli::cli_progress_step(paste0("Downloading data for variant ID: ", variantId, " ..."), spinner = TRUE)
    
    # Execute the query
    output0 <- con$exec(qry$queries$getUniProtVariantsData, variables)
    output1 <- jsonlite::fromJSON(output0, flatten = TRUE)
    
    if (length(output1$data$variant$evidences$rows) != 0) {
      final_output <- tibble::as_tibble(output1$data$variant$evidences$rows) %>%
        dplyr::mutate(
          variantId = output1$data$variant$id,
          referenceAllele = output1$data$variant$referenceAllele,
          alternateAllele = output1$data$variant$alternateAllele
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
