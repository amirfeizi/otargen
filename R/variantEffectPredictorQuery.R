#' Retrieve Variant Effect Predictor data for a specified variant.
#'
#' This function queries the Open Targets GraphQL API to retrieve variant effect predictor data
#' for a specified variant.
#'
#' @param variantId Character: ID of the target variant (e.g., "4_1804392_G_A").
#'
#' @return Returns a tibble containing variant effect predictor data for the specified variant.
#' @examples
#' \dontrun{
#' result <- variantEffectPredictorQuery(variantId = "4_1804392_G_A")
#' }
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble
#' @export
#'
variantEffectPredictorQuery <- function(variantId) {
  if (missing(variantId) || is.null(variantId)) {
    stop("Please provide a value for the 'variantId' argument.")
  }
  
  # Set up to query Open Targets API
  tryCatch({
    cli::cli_progress_step("Connecting to the Open Targets GraphQL API...", spinner = TRUE)
    con <- ghql::GraphqlClient$new("https://api.platform.opentargets.org/api/v4/graphql")
    qry <- ghql::Query$new()
    
    query <- "query VariantEffectPredictorQuery($variantId: String!) {
      variant(variantId: $variantId) {
        id
        transcriptConsequences {
          variantConsequences {
            id
            label
          }
          aminoAcidChange
          uniprotAccessions
          codons
          distanceFromFootprint
          distanceFromTss
          target {
            id
            approvedSymbol
            biotype
          }
          impact
          consequenceScore
          transcriptIndex
          transcriptId
          lofteePrediction
          siftPrediction
          polyphenPrediction
        }
        referenceAllele
        alternateAllele
      }
    }"
    
    variables <- list(
      variantId = variantId
    )
    
    qry$query(name = "getVariantEffectPredictorData", x = query)
    
    cli::cli_progress_step(paste0("Downloading data for variant ID: ", variantId, " ..."), spinner = TRUE)
    
    # Execute the query
    output0 <- con$exec(qry$queries$getVariantEffectPredictorData, variables)
    output1 <- jsonlite::fromJSON(output0, flatten = TRUE)
    
    if (length(output1$data$variant$transcriptConsequences) != 0) {
      final_output <- tibble::as_tibble(output1$data$variant$transcriptConsequences) %>%
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
