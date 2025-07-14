#' Retrieve Pharmacogenomics data for a specified drug.
#'
#' This function queries the Open Targets GraphQL API to retrieve pharmacogenomics data
#' for a specified drug.
#'
#' @param chemblId Character: ChEMBL ID of the target drug (e.g., "CHEMBL1016").
#'
#' @return Returns a tibble containing pharmacogenomics data for the specified drug.
#' @examples
#' \dontrun{
#' result <- pharmacogenomicsChemblQuery(chemblId = "CHEMBL1016")
#' }
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble
#' @export
#'
pharmacogenomicsChemblQuery <- function(chemblId) {
  if (missing(chemblId) || is.null(chemblId)) {
    stop("Please provide a value for the 'chemblId' argument.")
  }
  
  # Set up to query Open Targets API
  tryCatch({
    cli::cli_progress_step("Connecting to the Open Targets GraphQL API...", spinner = TRUE)
    con <- ghql::GraphqlClient$new("https://api.platform.opentargets.org/api/v4/graphql")
    qry <- ghql::Query$new()
    
    query <- "query PharmacogenomicsQuery($chemblId: String!) {
      drug(chemblId: $chemblId) {
        id
        pharmacogenomics {
          variantRsId
          genotypeId
          variantFunctionalConsequence {
            id
            label
          }
          target {
            id
            approvedSymbol
          }
          haplotypeId
          haplotypeFromSourceId
          isDirectTarget
          phenotypeFromSourceId
          genotypeAnnotationText
          phenotypeText
          pgxCategory
          evidenceLevel
          studyId
          literature
        }
      }
    }"
    
    variables <- list(
      chemblId = chemblId
    )
    
    qry$query(name = "getPharmacogenomicsData", x = query)
    
    cli::cli_progress_step(paste0("Downloading data for ChEMBL ID: ", chemblId, " ..."), spinner = TRUE)
    
    # Execute the query
    output0 <- con$exec(qry$queries$getPharmacogenomicsData, variables)
    output1 <- jsonlite::fromJSON(output0, flatten = TRUE)
    
    if (length(output1$data$drug$pharmacogenomics) != 0) {
      final_output <- tibble::as_tibble(output1$data$drug$pharmacogenomics) %>%
        dplyr::mutate(
          drugId = output1$data$drug$id
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
