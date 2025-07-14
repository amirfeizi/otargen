#' Retrieve QTL Credible Sets data for a specified variant.
#'
#' This function queries the Open Targets GraphQL API to retrieve QTL credible sets data
#' for a specified variant.
#'
#' @param variantId Character: ID of the target variant (e.g., "19_10352442_G_C").
#' @param size Integer: Number of records to retrieve (default: 500).
#' @param index Integer: Page index for pagination (default: 0).
#'
#' @return Returns a tibble containing QTL credible sets data for the specified variant.
#' @examples
#' \dontrun{
#' result <- qtlCredibleSetsQuery(variantId = "19_10352442_G_C", size = 500, 
#' index = 0)
#' }
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble
#' @export
#'
qtlCredibleSetsQuery <- function(variantId, size = 500, index = 0) {
  if (missing(variantId) || is.null(variantId)) {
    stop("Please provide a value for the 'variantId' argument.")
  }
  
  # Set up to query Open Targets API
  tryCatch({
    cli::cli_progress_step("Connecting to the Open Targets GraphQL API...", spinner = TRUE)
    con <- ghql::GraphqlClient$new("https://api.platform.opentargets.org/api/v4/graphql")
    qry <- ghql::Query$new()
    
    query <- "query QTLCredibleSetsQuery($variantId: String!, $size: Int!, $index: Int!) {
      variant(variantId: $variantId) {
        id
        referenceAllele
        alternateAllele
        qtlCredibleSets: credibleSets(
          studyTypes: [scsqtl, sceqtl, scpqtl, sctuqtl, sqtl, eqtl, pqtl, tuqtl]
          page: { size: $size, index: $index }
        ) {
          count
          rows {
            studyLocusId
            pValueMantissa
            pValueExponent
            beta
            finemappingMethod
            confidence
            isTransQtl
            variant {
              id
              chromosome
              position
              referenceAllele
              alternateAllele
            }
            study {
              id
              studyType
              condition
              target {
                id
                approvedSymbol
              }
              biosample {
                biosampleId
                biosampleName
              }
            }
            locus(variantIds: [$variantId]) {
              rows {
                posteriorProbability
              }
            }
            locusSize: locus {
              count
            }
          }
        }
      }
    }"
    
    variables <- list(
      variantId = variantId,
      size = size,
      index = index
    )
    
    qry$query(name = "getQTLCredibleSetsData", x = query)
    
    cli::cli_progress_step(paste0("Downloading data for variant ID: ", variantId, " ..."), spinner = TRUE)
    
    # Execute the query
    output0 <- con$exec(qry$queries$getQTLCredibleSetsData, variables)
    output1 <- jsonlite::fromJSON(output0, flatten = TRUE)
    
    if (length(output1$data$variant$qtlCredibleSets$rows) != 0) {
      final_output <- tibble::as_tibble(output1$data$variant$qtlCredibleSets$rows) %>%
        dplyr::mutate(
          variantId = output1$data$variant$id,
          referenceAllele = output1$data$variant$referenceAllele,
          alternateAllele = output1$data$variant$alternateAllele
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
