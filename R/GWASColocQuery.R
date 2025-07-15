#' Retrieve calculated GWAS colocalisation data
#'
#' This function retrieves colocalisation data for a specific study locus from a GWAS study with other GWAS studies.
#' It returns a data frame of the studies that colocalise with the input study locus,
#' including details on the study, reported trait, index variant, and calculated colocalisation method outputs.
#'
#' @param study_locus_id Character: Open Target Genetics generated ID for the study locus (e.g., "5a86bfd40d2ebecf6ce97bbe8a737512").
#' @param size Integer: Number of rows to fetch per page. Default: 500.
#' @param index Integer: Page index for pagination. Default: 0.
#'
#' @return Returns a data frame of the studies that colocalise with the input study locus. The table consists of the following data structure:
#' \itemize{
#'   \item{\code{study.studyId}:} \emph{Character vector}. Study identifier.
#'   \item{\code{study.traitReported}:} \emph{Character vector}. Reported trait associated with the colocalisation.
#'   \item{\code{study.projectId}:} \emph{Character vector}. Project identifier for the study.
#'   \item{\code{study.publicationFirstAuthor}:} \emph{Character vector}. First author of the publication.
#'   \item{\code{indexVariant.id}:} \emph{Character vector}. Index variant identifier.
#'   \item{\code{indexVariant.position}:} \emph{Integer vector}. Index variant position.
#'   \item{\code{indexVariant.chromosome}:} \emph{Character vector}. Index variant chromosome.
#'   \item{\code{indexVariant.referenceAllele}:} \emph{Character vector}. Reference allele of the variant.
#'   \item{\code{indexVariant.alternateAllele}:} \emph{Character vector}. Alternate allele of the variant.
#'   \item{\code{pValueMantissa}:} \emph{Numeric vector}. Mantissa of the p-value for the colocalisation.
#'   \item{\code{pValueExponent}:} \emph{Integer vector}. Exponent of the p-value for the colocalisation.
#'   \item{\code{numberColocalisingVariants}:} \emph{Integer vector}. Number of colocalising variants.
#'   \item{\code{colocalisationMethod}:} \emph{Character vector}. Method used for colocalisation analysis.
#'   \item{\code{h3}:} \emph{Numeric vector}. H3 value associated with the colocalisation.
#'   \item{\code{h4}:} \emph{Numeric vector}. H4 value associated with the colocalisation.
#'   \item{\code{clpp}:} \emph{Numeric vector}. Colocalisation posterior probability.
#'   \item{\code{betaRatioSignAverage}:} \emph{Numeric vector}. Average sign of the beta ratio.
#' }
#'
#' @examples
#' \dontrun{
#' colocalisation_data <- gwasColocalisation(study_locus_id = "5a86bfd40d2ebecf6ce97bbe8a737512",
#'  size = 500, index = 0)
#' }
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom ghql GraphqlClient Query
#' @importFrom jsonlite fromJSON
#' @importFrom cli cli_progress_step
#' @export
#' @references
#' Giambartolomei, Claudia et al. “Bayesian test for colocalisation between pairs of genetic association studies using summary statistics.” PLoS genetics vol. 10,5 e1004383. 15 May. 2014, doi:10.1371/journal.pgen.1004383
#'
gwasColocalisation <- function(study_locus_id, size = 500, index = 0) {
  # Validate inputs
  if (missing(study_locus_id) || is.null(study_locus_id) || !is.character(study_locus_id)) {
    stop("Please provide a valid study_locus_id (character string).")
  }
  if (!is.numeric(size) || size < 1) {
    stop("Size must be a positive integer.")
  }
  if (!is.numeric(index) || index < 0) {
    stop("Index must be a non-negative integer.")
  }
  
  # Set up GraphQL client
  cli::cli_progress_step("Connecting to the Open Targets Genetics GraphQL API...", spinner = TRUE)
  otg_cli <- ghql::GraphqlClient$new(url = "https://api.platform.opentargets.org/api/v4/graphql")
  otg_qry <- ghql::Query$new()
  
  # Define GraphQL query
  query <- '
    query GWASColocQuery($studyLocusId: String!, $size: Int!, $index: Int!) {
      credibleSet(studyLocusId: $studyLocusId) {
        colocalisation(studyTypes: [gwas], page: { size: $size, index: $index }) {
          count
          rows {
            otherStudyLocus {
              studyLocusId
              study {
                id
                projectId
                traitFromSource
                publicationFirstAuthor
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
            }
            numberColocalisingVariants
            colocalisationMethod
            h3
            h4
            clpp
            betaRatioSignAverage
          }
        }
      }
    }
  '
  
  # Execute the query
  variables <- list(studyLocusId = study_locus_id, size = size, index = index)
  otg_qry$query(name = "gwascol_query", x = query)
  cli::cli_progress_step("Downloading data...", spinner = TRUE)
  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$gwascol_query, variables), flatten = TRUE)$data
  
  # Process the response
  rows <- result$credibleSet$colocalisation$rows
  if (is.null(rows) || length(rows) == 0) {
    message("No colocalisation data found for the given study locus ID.")
    return(data.frame())
  }
  
  # Convert to data frame and select relevant columns
  output <- rows %>%
    dplyr::tibble() %>%
    dplyr::select(
      study.studyId = otherStudyLocus.study.id,
      study.projectId = otherStudyLocus.study.projectId,
      study.traitReported = otherStudyLocus.study.traitFromSource,
      study.publicationFirstAuthor = otherStudyLocus.study.publicationFirstAuthor,
      indexVariant.id = otherStudyLocus.variant.id,
      indexVariant.chromosome = otherStudyLocus.variant.chromosome,
      indexVariant.position = otherStudyLocus.variant.position,
      indexVariant.referenceAllele = otherStudyLocus.variant.referenceAllele,
      indexVariant.alternateAllele = otherStudyLocus.variant.alternateAllele,
      pValueMantissa = otherStudyLocus.pValueMantissa,
      pValueExponent = otherStudyLocus.pValueExponent,
      numberColocalisingVariants,
      colocalisationMethod,
      h3,
      h4,
      clpp,
      betaRatioSignAverage
    )
  
  return(output)
}
