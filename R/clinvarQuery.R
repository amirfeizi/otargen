#' Retrieve ClinVar data for a specified gene and disease.
#'
#' This function queries the Open Targets Genetics GraphQL API to retrieve ClinVar data
#' for a specified gene and disease, including evidence from the NCBI datasource.
#'
#' @param ensemblId Character: ENSEMBL ID of the target gene (e.g., ENSG00000130164).
#' @param efoId Character: EFO ID of the disease (e.g., EFO_0004911).
#' @param size Integer: Number of records to retrieve (default: 10).
#' @param cursor Character: Cursor for pagination (default: NULL).
#'
#' @return Returns a data frame containing ClinVar data for the specified gene and disease.
#' @examples
#' \dontrun{
#' result <- clinvarQuery(ensemblId = "ENSG00000130164",
#'  efoId = "EFO_0004911", size = 10)
#' result <- clinvarQuery(ensemblId = "ENSG00000130164",
#'  efoId = "EFO_0004911", size = 10, cursor = NULL)
#' }
#' @importFrom magrittr %>%
#' @export
#'
clinvarQuery <- function(ensemblId, efoId, size = 10, cursor = NULL) {
  if (missing(ensemblId) || is.null(ensemblId)) {
    stop("Please provide a value for the 'ensemblId' argument.")
  }
  if (missing(efoId) || is.null(efoId)) {
    stop("Please provide a value for the 'efoId' argument.")
  }

  # Set up to query Open Targets Genetics API
  tryCatch({
    cli::cli_progress_step("Connecting to the Open Targets Genetics GraphQL API...", spinner = TRUE)
    con <- ghql::GraphqlClient$new("https://api.platform.opentargets.org/api/v4/graphql")
    qry <- ghql::Query$new()

    query <- "query ClinvarQuery($ensemblId: String!, $efoId: String!, $size: Int!, $cursor: String) {
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
          cursor: $cursor
        ) {
          cursor
          count
          rows {
            disease {
              id
              name
            }
            variantEffect
            directionOnTrait
            diseaseFromSource
            variantId
            variantRsId
            variantHgvsId
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

    variables <- list(
      ensemblId = ensemblId,
      efoId = efoId,
      size = size,
      cursor = cursor
    )

    qry$query(name = "getClinvarData", x = query)

    cli::cli_progress_step(paste0("Downloading data for ENSEMBL ID: ", ensemblId, " and EFO ID: ", efoId, " ..."), spinner = TRUE)

    # Execute the query
    output0 <- con$exec(qry$queries$getClinvarData, variables)
    output1 <- jsonlite::fromJSON(output0, flatten = TRUE)

    if (length(output1$data$disease$eva$rows) != 0) {
      final_output <- output1$data$disease$eva$rows
      final_output$approvedSymbol <- output1$data$target$approvedSymbol
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
