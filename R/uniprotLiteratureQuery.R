#' Retrieve UniProt Literature data for a specified gene and disease.
#'
#' This function queries the Open Targets GraphQL API to retrieve UniProt literature evidence data
#' for a specified gene and disease.
#'
#' @param ensemblId Character: ENSEMBL ID of the target gene (e.g., "ENSG00000130203").
#' @param efoId Character: EFO ID of the target disease (e.g., "MONDO_0004975").
#' @param size Integer: Number of records to retrieve (default: 3500).
#'
#' @return Returns a tibble containing UniProt literature evidence data for the specified gene and disease.
#' @examples
#' \dontrun{
#' result <- uniprotLiteratureQuery(ensemblId = "ENSG00000130203", efoId = 
#' "MONDO_0004975", size = 3500)
#' }
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble
#' @export
#'
uniprotLiteratureQuery <- function(ensemblId, efoId, size = 3500) {
  if (missing(ensemblId) || is.null(ensemblId)) {
    stop("Please provide a value for the 'ensemblId' argument.")
  }
  if (missing(efoId) || is.null(efoId)) {
    stop("Please provide a value for the 'efoId' argument.")
  }
  
  # Set up to query Open Targets API
  tryCatch({
    cli::cli_progress_step("Connecting to the Open Targets GraphQL API...", spinner = TRUE)
    con <- ghql::GraphqlClient$new("https://api.platform.opentargets.org/api/v4/graphql")
    qry <- ghql::Query$new()
    
    query <- "query UniprotLiteratureQuery($ensemblId: String!, $efoId: String!, $size: Int!) {
      target(ensemblId: $ensemblId) {
        approvedSymbol
      }
      disease(efoId: $efoId) {
        id
        uniprotLiteratureSummary: evidences(
          ensemblIds: [$ensemblId]
          enableIndirect: true
          datasourceIds: [\"uniprot_literature\"]
          size: $size
        ) {
          count
          rows {
            disease {
              id
              name
            }
            diseaseFromSource
            targetFromSourceId
            studyId
            literature
            confidence
          }
        }
      }
    }"
    
    variables <- list(
      ensemblId = ensemblId,
      efoId = efoId,
      size = size
    )
    
    qry$query(name = "getUniprotLiteratureData", x = query)
    
    cli::cli_progress_step(paste0("Downloading data for ENSEMBL ID: ", ensemblId, " and EFO ID: ", efoId, " ..."), spinner = TRUE)
    
    # Execute the query
    output0 <- con$exec(qry$queries$getUniprotLiteratureData, variables)
    output1 <- jsonlite::fromJSON(output0, flatten = TRUE)
    
    if (length(output1$data$disease$uniprotLiteratureSummary$rows) != 0) {
      final_output <- tibble::as_tibble(output1$data$disease$uniprotLiteratureSummary$rows) %>%
        dplyr::mutate(
          approvedSymbol = output1$data$target$approvedSymbol,
          diseaseId = output1$data$disease$id,
          uniprotLiteratureCount = output1$data$disease$uniprotLiteratureSummary$count
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
