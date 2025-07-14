#' Retrieve Cancer Hallmarks data for a specified gene.
#'
#' This function queries the Open Targets GraphQL API to retrieve cancer hallmarks data
#' for a specified gene.
#'
#' @param ensgId Character: ENSEMBL ID of the target gene (e.g., ENSG00000141510).
#'
#' @return Returns a tibble containing cancer hallmarks data for the specified gene.
#' @examples
#' \dontrun{
#' result <- hallmarksQuery(ensgId = "ENSG00000141510")
#' }
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble
#' @importFrom tidyr unnest
#' @importFrom dplyr bind_rows
#' @export
#'
hallmarksQuery <- function(ensgId) {
  if (missing(ensgId) || is.null(ensgId)) {
    stop("Please provide a value for the 'ensgId' argument.")
  }
  
  # Set up to query Open Targets API
  tryCatch({
    cli::cli_progress_step("Connecting to the Open Targets GraphQL API...", spinner = TRUE)
    con <- ghql::GraphqlClient$new("https://api.platform.opentargets.org/api/v4/graphql")
    qry <- ghql::Query$new()
    
    query <- "query HallmarksQuery($ensgId: String!) {
      target(ensemblId: $ensgId) {
        id
        hallmarks {
          attributes {
            name
            pmid
            description
          }
          cancerHallmarks {
            pmid
            impact
            description
            label
          }
        }
      }
    }"
    
    variables <- list(
      ensgId = ensgId
    )
    
    qry$query(name = "getHallmarksData", x = query)
    
    cli::cli_progress_step(paste0("Downloading data for ENSEMBL ID: ", ensgId, " ..."), spinner = TRUE)
    
    # Execute the query
    output0 <- con$exec(qry$queries$getHallmarksData, variables)
    output1 <- jsonlite::fromJSON(output0, flatten = TRUE)
    
    if (length(output1$data$target$hallmarks) != 0) {
      # Process attributes and cancerHallmarks separately
      attributes <- if (length(output1$data$target$hallmarks$attributes) != 0) {
        tibble::as_tibble(output1$data$target$hallmarks$attributes) %>%
          dplyr::mutate(type = "attributes")
      } else {
        tibble::tibble(name = character(), pmid = integer(), description = character(), type = character())
      }
      
      cancerHallmarks <- if (length(output1$data$target$hallmarks$cancerHallmarks) != 0) {
        tibble::as_tibble(output1$data$target$hallmarks$cancerHallmarks) %>%
          dplyr::mutate(type = "cancerHallmarks")
      } else {
        tibble::tibble(pmid = integer(), impact = character(), description = character(), label = character(), type = character())
      }
      
      # Combine the results into a single tibble
      final_output <- dplyr::bind_rows(attributes, cancerHallmarks) %>%
        dplyr::mutate(geneId = output1$data$target$id)
      
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
