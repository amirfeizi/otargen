#' Retrieve Genetic Constraint data for a specified gene.
#'
#' This function queries the Open Targets Platform GraphQL API to retrieve genetic constraint data
#' for a specified gene, such as pLI or LOEUF scores.
#'
#' @param ensgId Character: ENSEMBL ID of the target gene (e.g., "ENSG00000141510").
#'
#' @return Returns a tibble containing genetic constraint data for the specified gene.
#' @examples
#' \dontrun{
#' result <- geneticConstraintQuery(ensgId = "ENSG00000141510")
#' }
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate
#' @importFrom ghql GraphqlClient
#' @importFrom jsonlite fromJSON
#' @importFrom cli cli_progress_step
#' @export
#'
geneticConstraintQuery <- function(ensgId) {
  if (missing(ensgId) || is.null(ensgId)) {
    stop("Please provide a value for the 'ensgId' argument.")
  }
  
  # Set up to query Open Targets Platform API
  tryCatch({
    cli::cli_progress_step("Connecting to the Open Targets Platform GraphQL API...", spinner = TRUE)
    con <- ghql::GraphqlClient$new("https://api.platform.opentargets.org/api/v4/graphql")
    qry <- ghql::Query$new()
    
    query <- "query GeneticConstraintQuery($ensgId: String!) {
      target(ensemblId: $ensgId) {
        id
        approvedSymbol
        geneticConstraint {
          constraintType
          score
          upperBin
          upperBin6
        }
      }
    }"
    
    variables <- list(
      ensgId = ensgId
    )
    
    qry$query(name = "getGeneticConstraintData", x = query)
    
    cli::cli_progress_step(paste0("Downloading data for ENSEMBL ID: ", ensgId, " ..."), spinner = TRUE)
    
    # Execute the query
    output0 <- con$exec(qry$queries$getGeneticConstraintData, variables)
    output1 <- jsonlite::fromJSON(output0, flatten = TRUE)
    
    if (length(output1$data$target$geneticConstraint) != 0) {
      final_output <- tibble::as_tibble(output1$data$target$geneticConstraint) %>%
        dplyr::mutate(
          geneId = output1$data$target$id,
          approvedSymbol = output1$data$target$approvedSymbol
        )
      return(final_output)
    } else {
      message("No data found for the given parameters.")
      return(NULL)
    }
    
  }, error = function(e) {
    # Handle connection timeout
    if (grepl("Timeout was reached", e$message)) {
      stop("Connection timeout reached while connecting to the Open Targets Platform GraphQL API.")
    } else {
      stop(e) # Handle other types of errors
    }
  })
}