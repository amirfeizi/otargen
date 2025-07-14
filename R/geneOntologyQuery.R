#' Retrieve Gene Ontology data for a specified gene.
#'
#' This function queries the Open Targets GraphQL API to retrieve gene ontology data
#' for a specified gene.
#'
#' @param ensgId Character: ENSEMBL ID of the target gene (e.g., ENSG00000141510).
#'
#' @return Returns a tibble containing gene ontology data for the specified gene.
#' @examples
#' \dontrun{
#' result <- geneOntologyQuery(ensgId = "ENSG00000141510")
#' }
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble
#' @export
#'
geneOntologyQuery <- function(ensgId) {
  if (missing(ensgId) || is.null(ensgId)) {
    stop("Please provide a value for the 'ensgId' argument.")
  }
  
  # Set up to query Open Targets API
  tryCatch({
    cli::cli_progress_step("Connecting to the Open Targets GraphQL API...", spinner = TRUE)
    con <- ghql::GraphqlClient$new("https://api.platform.opentargets.org/api/v4/graphql")
    qry <- ghql::Query$new()
    
    query <- "query GeneOntologyQuery($ensgId: String!) {
      target(ensemblId: $ensgId) {
        id
        geneOntology {
          term {
            id
            name
          }
          aspect
          evidence
          geneProduct
          source
        }
      }
    }"
    
    variables <- list(
      ensgId = ensgId
    )
    
    qry$query(name = "getGeneOntologyData", x = query)
    
    cli::cli_progress_step(paste0("Downloading data for ENSEMBL ID: ", ensgId, " ..."), spinner = TRUE)
    
    # Execute the query
    output0 <- con$exec(qry$queries$getGeneOntologyData, variables)
    output1 <- jsonlite::fromJSON(output0, flatten = TRUE)
    
    if (length(output1$data$target$geneOntology) != 0) {
      final_output <- tibble::as_tibble(output1$data$target$geneOntology)
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
