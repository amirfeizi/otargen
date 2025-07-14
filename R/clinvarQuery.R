#' Retrieve ClinVar data for a specified gene and disease.
#'
#' This function queries the Open Targets GraphQL API to retrieve ClinVar evidence data
#' for a specified gene and disease.
#'
#' @param ensemblId Character: ENSEMBL ID of the target gene (e.g., "ENSG00000080815").
#' @param efoId Character: EFO ID of the target disease (e.g., "MONDO_0004975").
#' @param cursor Character: Cursor for pagination (default: NULL).
#' @param size Integer: Number of records to retrieve (default: 10).
#'
#' @return Returns a tibble containing ClinVar evidence data for the specified gene and disease.
#' @examples
#' \dontrun{
#' result <- clinVarQuery(ensemblId = "ENSG00000080815", efoId = 
#' "MONDO_0004975", size = 10)
#' result <- clinVarQuery(ensemblId = "ENSG00000080815", efoId = 
#' "MONDO_0004975", cursor = NULL, size = 10)
#' }
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble
#' @export
#'
clinVarQuery <- function(ensemblId, efoId, cursor = NULL, size = 10) {
  if (missing(ensemblId) || is.null(ensemblId)) {
    stop("Please provide a value for the 'ensemblId' argument.")
  }
  if (missing(efoId) || is.null(efoId)) {
    stop("Please provide a value for the 'efoId' argument.")
  }
  
  # Set up to query Open Targets  API
  tryCatch({
    cli::cli_progress_step("Connecting to the Open Targets  GraphQL API...", spinner = TRUE)
    con <- ghql::GraphqlClient$new("https://api.platform.opentargets.org/api/v4/graphql")
    qry <- ghql::Query$new()
    
    # Define base query with placeholders
    query <- "query ClinvarQuery($ensemblId: String!, $efoId: String!, $size: Int!{optional_cursor}) {
      target(ensemblId: $ensemblId) {
        approvedSymbol
      }
      disease(efoId: $efoId) {
        id
        name
        eva: evidences(
          ensemblIds: [$ensemblId]
          enableIndirect: true
          datasourceIds: [\"eva\"]
          size: $size
          {cursor_param}
        ) {
          cursor
          count
          rows {
            disease {
              id
              name
            }
            variant {
              id
              hgvsId
              referenceAllele
              alternateAllele
            }
            variantEffect
            directionOnTrait
            diseaseFromSource
            variantRsId
            studyId
            variantFunctionalConsequence {
              id
              label
            }
            clinicalSignificances
            allelicRequirements
            alleleOrigins
            confidence
            literature
            cohortPhenotypes
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
    
    qry$query(name = "getClinvarData", x = query)
    
    cli::cli_progress_step(paste0("Downloading data for ENSEMBL ID: ", ensemblId, " and EFO ID: ", efoId, " ..."), spinner = TRUE)
    
    # Execute the query
    output0 <- con$exec(qry$queries$getClinvarData, variables)
    output1 <- jsonlite::fromJSON(output0, flatten = TRUE)
    
    if (length(output1$data$disease$eva$rows) != 0) {
      final_output <- tibble::as_tibble(output1$data$disease$eva$rows) %>%
        dplyr::mutate(
          approvedSymbol = output1$data$target$approvedSymbol,
          diseaseId = output1$data$disease$id,
          diseaseName = output1$data$disease$name,
          evaCount = output1$data$disease$eva$count,
          cursor = output1$data$disease$eva$cursor
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
