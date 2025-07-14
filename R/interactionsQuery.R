#' Retrieve Interactions data for a specified gene.
#'
#' This function queries the Open Targets GraphQL API to retrieve molecular interaction data
#' for a specified gene.
#'
#' @param ensgId Character: ENSEMBL ID of the target gene (e.g., ENSG00000141510).
#' @param sourceDatabase Character: Source database for interactions (e.g., "intact") (default: NULL).
#' @param index Integer: Page index for pagination (default: 0).
#' @param size Integer: Number of records to retrieve (default: 10).
#'
#' @return Returns a tibble containing interactions data for the specified gene.
#' @examples
#' \dontrun{
#' result <- interactionsQuery(ensgId = "ENSG00000141510",
#'  sourceDatabase = "intact", size = 10)
#' result <- interactionsQuery(ensgId = "ENSG00000141510",
#'  sourceDatabase = "intact", index = 0, size = 10)
#' }
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble
#' @export
#'
interactionsQuery <- function(ensgId, sourceDatabase = NULL, index = 0, size = 10) {
  if (missing(ensgId) || is.null(ensgId)) {
    stop("Please provide a value for the 'ensgId' argument.")
  }
  
  # Set up to query Open Targets  API
  tryCatch({
    cli::cli_progress_step("Connecting to the Open Targets  GraphQL API...", spinner = TRUE)
    con <- ghql::GraphqlClient$new("https://api.platform.opentargets.org/api/v4/graphql")
    qry <- ghql::Query$new()
    
    # Define base query
    query <- "query InteractionsSectionQuery($ensgId: String!, $sourceDatabase: String, $index: Int = 0, $size: Int = 10) {
      target(ensemblId: $ensgId) {
        id
        approvedName
        approvedSymbol
        interactions(sourceDatabase: $sourceDatabase, page: { index: $index, size: $size }) {
          count
          rows {
            intA
            intABiologicalRole
            targetA {
              id
              approvedSymbol
            }
            speciesA {
              mnemonic
            }
            intB
            intBBiologicalRole
            targetB {
              id
              approvedSymbol
            }
            speciesB {
              mnemonic
            }
            score
            count
            sourceDatabase
            evidences {
              evidenceScore
              hostOrganismScientificName
              interactionDetectionMethodMiIdentifier
              interactionDetectionMethodShortName
              interactionIdentifier
              interactionTypeShortName
              participantDetectionMethodA {
                miIdentifier
                shortName
              }
              participantDetectionMethodB {
                miIdentifier
                shortName
              }
              interactionDetectionMethodShortName
              expansionMethodShortName
              pubmedId
            }
          }
        }
      }
    }"
    
    variables <- list(
      ensgId = ensgId,
      sourceDatabase = sourceDatabase,
      index = index,
      size = size
    )
    
    qry$query(name = "getInteractionsData", x = query)
    
    cli::cli_progress_step(paste0("Downloading data for ENSEMBL ID: ", ensgId, " ..."), spinner = TRUE)
    
    # Execute the query
    output0 <- con$exec(qry$queries$getInteractionsData, variables)
    output1 <- jsonlite::fromJSON(output0, flatten = TRUE)
    
    if (length(output1$data$target$interactions$rows) != 0) {
      final_output <- tibble::as_tibble(output1$data$target$interactions$rows)
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
