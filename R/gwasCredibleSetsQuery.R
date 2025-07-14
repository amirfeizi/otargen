#' Retrieve GWAS Credible Sets data for a specified target and disease.
#'
#' This function queries the Open Targets GraphQL API to retrieve GWAS credible sets
#' evidence data for a specified target gene and disease.
#'
#' @param ensemblId Character. Ensembl gene ID, e.g. "ENSG00000105397".
#' @param efoId Character. EFO disease ID, e.g. "EFO_0000685".
#' @param size Integer. Number of rows to fetch. Default: 500.
#'
#' @return A tibble with credible set evidence or NULL if no data found.
#'
#' @examples
#' \dontrun{
#'   # Example call:
#'   result <- gwasCredibleSetsQuery(
#'     ensemblId = "ENSG00000105397",
#'     efoId = "EFO_0000685",
#'     size = 5
#'   )
#'   print(result)
#' }
#'
#' @export
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
  
  if (is.null(rows) || length(rows) == 0) {
    message("No GWAS credible sets data found for the given parameters.")
    return(NULL)
  }
  
  rows_df <- tibble::as_tibble(rows)
  
  if (!"credibleSet" %in% names(rows_df)) {
    message("No credibleSet data available in the response.")
    return(NULL)
  }
  
  credible_data <- rows_df %>%
    tidyr::unnest_wider(credibleSet, names_sep = ".") %>%
    tidyr::unnest_wider(credibleSet.study, names_sep = ".") %>%
    tidyr::unnest_wider(credibleSet.variant, names_sep = ".") %>%
    dplyr::mutate(
      targetEnsemblId = ensemblId,
      targetSymbol = output$data$target$approvedSymbol,
      diseaseId = output$data$disease$id,
      diseaseName = output$data$disease$name
    )
  
  return(credible_data)
}
