#' Retrieve Comparative Genomics data for a specified gene.
#'
#' This function queries the Open Targets Genetics GraphQL API to retrieve comparative genomics data
#' for a specified gene.
#'
#' @param ensemblId Character: ENSEMBL ID of the target gene (e.g., ENSG00000169174).
#'
#' @return Returns a data frame containing comparative genomics data for the specified gene.
#' @examples
#' \dontrun{
#' result <- compGenomicsQuery(ensemblId = "ENSG00000169174")
#' }
#' @importFrom magrittr %>%
#' @export
#'
compGenomicsQuery <- function(ensemblId) {
  if (missing(ensemblId) || is.null(ensemblId)) {
    stop("Please provide a value for the 'ensemblId' argument.")
  }

  # Set up to query Open Targets Genetics API
  tryCatch({
    cli::cli_progress_step("Connecting to the Open Targets Genetics GraphQL API...", spinner = TRUE)
    con <- ghql::GraphqlClient$new("https://api.platform.opentargets.org/api/v4/graphql")
    qry <- ghql::Query$new()

    query <- "query CompGenomics($ensemblId: String!) {
      target(ensemblId: $ensemblId) {
        id
        homologues {
          speciesId
          speciesName
          homologyType
          isHighConfidence
          targetGeneId
          targetGeneSymbol
          queryPercentageIdentity
          targetPercentageIdentity
        }
      }
    }"

    variables <- list(
      ensemblId = ensemblId
    )

    qry$query(name = "getCompGenomicsData", x = query)

    cli::cli_progress_step(paste0("Downloading data for ENSEMBL ID: ", ensemblId, " ..."), spinner = TRUE)

    # Execute the query
    output0 <- con$exec(qry$queries$getCompGenomicsData, variables)
    output1 <- jsonlite::fromJSON(output0, flatten = TRUE)

    if (!is.null(output1$data$target$homologues)) {
      final_output <- output1$data$target$homologues
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
