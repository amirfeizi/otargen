#' Retrieve GWAS Credible Sets data for a specified target and disease.
#'
#' This function queries the Open Targets Platform GraphQL API to retrieve GWAS credible sets
#' evidence data for a specified target gene and disease.
#'
#' @param ensemblId Character. Ensembl gene ID, e.g., "ENSG00000169174".
#' @param efoId Character. EFO disease ID, e.g., "EFO_0004911".
#' @param size Integer. Number of rows to fetch. Default: 500.
#'
#' @return A tibble with credible set evidence or NULL if no data found.
#'
#' @examples
#' \dontrun{
#'   result <- gwasCredibleSetsQuery(
#'     ensemblId = "ENSG00000169174",
#'     efoId = "EFO_0004911",
#'     size = 5
#'   )
#'   print(result)
#' }
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate
#' @importFrom tidyr unnest_wider
#' @importFrom httr POST add_headers content
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom cli cli_progress_step
#' @export
#'
gwasCredibleSetsQuery <- function(ensemblId, efoId, size = 500) {
  if (missing(ensemblId) || is.null(ensemblId)) {
    stop("Please provide a value for 'ensemblId'.")
  }
  if (missing(efoId) || is.null(efoId)) {
    stop("Please provide a value for 'efoId'.")
  }
  
  query <- '
    query GwasCredibleSetsQuery($ensemblId: String!, $efoId: String!, $size: Int!) {
      target(ensemblId: $ensemblId) {
        approvedSymbol
      }
      disease(efoId: $efoId) {
        id
        name
        gwasCredibleSets: evidences(
          ensemblIds: [$ensemblId]
          enableIndirect: true
          datasourceIds: ["gwas_credible_sets"]
          size: $size
        ) {
          count
          rows {
            disease {
              id
              name
            }
            credibleSet {
              studyLocusId
              study {
                traitFromSource
                id
                projectId
                publicationFirstAuthor
                publicationDate
                pubmedId
                nSamples
              }
              variant {
                id
                chromosome
                position
                referenceAllele
                alternateAllele
              }
              pValueMantissa
              pValueExponent
              beta
              finemappingMethod
              confidence
            }
            score
          }
        }
      }
    }
  '
  
  cli::cli_progress_step("Sending GraphQL request...", spinner = TRUE)
  
  resp <- httr::POST(
    url = "https://api.platform.opentargets.org/api/v4/graphql",
    httr::add_headers(`Content-Type` = "application/json"),
    body = jsonlite::toJSON(
      list(
        query = query,
        variables = list(
          ensemblId = ensemblId,
          efoId = efoId,
          size = size
        )
      ),
      auto_unbox = TRUE
    )
  )
  
  if (httr::status_code(resp) != 200) {
    stop("GraphQL query failed. HTTP status: ", httr::status_code(resp))
  }
  
  output <- jsonlite::fromJSON(httr::content(resp, as = "text"), flatten = TRUE)
  
  rows <- output$data$disease$gwasCredibleSets$rows
  
  if (is.null(rows) || nrow(tibble::as_tibble(rows)) == 0) {
    message("No GWAS credible sets data found for the given parameters.")
    return(NULL)
  }
  
  # Convert rows to tibble and check for credible set fields
  rows_df <- tibble::as_tibble(rows)
  
  # Check if credible set fields exist in the flattened data
  if (!any(grepl("^credibleSet\\.", names(rows_df)))) {
    message("No credibleSet data available in the response.")
    return(NULL)
  }
  
  # Unnest flattened credible set fields
  credible_data <- rows_df %>%
    dplyr::mutate(
      targetEnsemblId = ensemblId,
      targetSymbol = output$data$target$approvedSymbol,
      diseaseId = output$data$disease$id,
      diseaseName = output$data$disease$name
    )
  
  return(credible_data)
}
