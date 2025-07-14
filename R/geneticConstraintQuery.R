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
    query GwasCredibleSetsQuery($ensemblId: String!, $ensemblIds: [String!]!, $efoId: String!, $size: Int!) {
      target(ensemblId: $ensemblId) {
        approvedSymbol
      }
      disease(efoId: $efoId) {
        id
        name
        gwasCredibleSets: evidences(
          ensemblIds: $ensemblIds
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
  
  variables <- list(
    ensemblId = ensemblId,
    ensemblIds = list(ensemblId),
    efoId = efoId,
    size = size
  )
  
  body <- list(
    query = query,
    variables = variables
  )
  
  cli::cli_progress_step("Sending GraphQL request...", spinner = TRUE)
  
  # Make the POST request to the Open Targets GraphQL API
  resp <- httr::POST(
    url = "https://api.platform.opentargets.org/api/v4/graphql",
    httr::add_headers(`Content-Type` = "application/json"),
    body = jsonlite::toJSON(body, auto_unbox = TRUE)
  )
  
  # Capture the raw JSON response
  json_txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  
  # Parse the JSON response
  output <- jsonlite::fromJSON(json_txt, flatten = TRUE)
  
  # Check for errors in the response
  if (!is.null(output$errors)) {
    print(output$errors)
    stop("GraphQL returned errors.")
  }
  
  # Access the rows directly
  rows <- output$data$disease$gwasCredibleSets$rows
  
  if (is.null(rows) || length(rows) == 0) {
    message("No GWAS credible sets data found for the given parameters.")
    return(NULL)
  }
  
  # Process and unnest the data manually
  credible_data <- tibble::as_tibble(rows) %>%
    dplyr::mutate(
      # Directly access nested fields in `credibleSet`
      studyLocusId = `credibleSet.studyLocusId`,
      pValueMantissa = `credibleSet.pValueMantissa`,
      pValueExponent = `credibleSet.pValueExponent`,
      beta = `credibleSet.beta`,
      finemappingMethod = `credibleSet.finemappingMethod`,
      confidence = `credibleSet.confidence`,
      # Handle nested study and variant data
      study.traitFromSource = `credibleSet.study.traitFromSource`,
      study.id = `credibleSet.study.id`,
      study.projectId = `credibleSet.study.projectId`,
      study.publicationFirstAuthor = `credibleSet.study.publicationFirstAuthor`,
      study.publicationDate = `credibleSet.study.publicationDate`,
      study.pubmedId = `credibleSet.study.pubmedId`,
      study.nSamples = `credibleSet.study.nSamples`,
      variant.id = `credibleSet.variant.id`,
      variant.chromosome = `credibleSet.variant.chromosome`,
      variant.position = `credibleSet.variant.position`,
      variant.referenceAllele = `credibleSet.variant.referenceAllele`,
      variant.alternateAllele = `credibleSet.variant.alternateAllele`,
      # Add other fields as needed
      diseaseId = `disease.id`,
      diseaseName = `disease.name`,
      score = `score`
    ) %>%
    dplyr::select(-starts_with("credibleSet"))  # Remove redundant `credibleSet` columns
  
  return(credible_data)
}


