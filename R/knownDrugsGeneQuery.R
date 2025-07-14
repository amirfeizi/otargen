#' Retrieve Known Drugs data for a specified gene.
#'
#' This function queries the Open Targets GraphQL API to retrieve known drugs data
#' for a specified gene.
#'
#' @param ensgId Character: ENSEMBL ID of the target gene (e.g., ENSG00000169174).
#' @param cursor Character: Cursor for pagination (default: NULL).
#' @param freeTextQuery Character: Free text query to filter results (default: NULL).
#' @param size Integer: Number of records to retrieve (default: 10).
#'
#' @return Returns a data frame containing known drugs data for the specified gene.
#' @examples
#' \dontrun{
#' result <- knownDrugsGeneQuery(ensgId = "ENSG00000169174", size = 10)
#' result <- knownDrugsGeneQuery(ensgId = "ENSG00000169174",
#'  cursor = NULL, freeTextQuery = NULL, size = 10)
#' }
#' @importFrom magrittr %>%
#' @export
#'
knownDrugsGeneQuery <- function(ensgId, cursor = NULL, freeTextQuery = NULL, size = 10) {
  if (missing(ensgId) || is.null(ensgId)) {
    stop("Please provide a value for the 'ensgId' argument.")
  }

  # Set up to query Open Targets API
  tryCatch({
    cli::cli_progress_step("Connecting to the Open Targets GraphQL API...", spinner = TRUE)
    con <- ghql::GraphqlClient$new("https://api.platform.opentargets.org/api/v4/graphql")
    qry <- ghql::Query$new()

    query <- "query KnownDrugsQuery($ensgId: String!, $size: Int = 10) {
      target(ensemblId: $ensgId) {
        id
        knownDrugs(size: $size) {
          count
          cursor
          rows {
            phase
            status
            urls {
              name
              url
            }
            disease {
              id
              name
            }
            drug {
              id
              name
              mechanismsOfAction {
                rows {
                  actionType
                  targets {
                    id
                  }
                }
              }
            }
            drugType
            mechanismOfAction
          }
        }
      }
    }"

    variables <- list(
      ensgId = ensgId,
      cursor = cursor,
      freeTextQuery = freeTextQuery,
      size = size
    )

    qry$query(name = "getKnownDrugsData", x = query)

    cli::cli_progress_step(paste0("Downloading data for ENSEMBL ID: ", ensgId, " ..."), spinner = TRUE)

    # Execute the query
    output0 <- con$exec(qry$queries$getKnownDrugsData, variables)
    output1 <- jsonlite::fromJSON(output0, flatten = TRUE)

    if (length(output1$data$target$knownDrugs$rows) != 0) {
      final_output <- output1$data$target$knownDrugs$rows
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
