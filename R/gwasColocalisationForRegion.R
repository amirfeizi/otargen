#' Retrieve GWAS colocalisation data for a genomic region .
#'
#' By providing a genomic region (chromosome name with start and end position), this function returns information about colocalisation
#' between GWAS studies and associated loci within a specified genomic region. It provides details on the
#' studies that have at least one overlapping associated locus within the region, allowing for the assessment
#' of potential shared causal variants.
#' The query output includes data such as the study identifiers, traits,
#' loci information, and other relevant attributes.
#'
#' @param chromosome String: Chromosome number as a string.
#' @param start Long: Start position of the specified chromosome.
#' @param end Long: End position of the specified chromosome.
#'
#' @return Returns a data frame with the following columns:
#' \itemize{
#' \item{\code{leftVariant.id}:} \emph{Character vector}. ID of the left variant.
#' \item{\code{leftVariant.position}:} \emph{Integer vector}. Position of the left variant.
#' \item{\code{leftVariant.chromosome}:} \emph{Character vector}. Chromosome of the left variant.
#' \item{\code{leftVariant.rsId}:} \emph{Character vector}. rsID of the left variant.
#' \item{\code{leftStudy.studyId}:} \emph{Character vector}. Study identifier for the left study.
#' \item{\code{leftStudy.traitReported}:} \emph{Character vector}. Reported trait associated with the colocalisation in the left study.
#' \item{\code{leftStudy.traitCategory}:} \emph{Character vector}. Category of the reported trait in the left study.
#' \item{\code{rightVariant.id}:} \emph{Character vector}. ID of the right variant.
#' \item{\code{rightVariant.position}:} \emph{Integer vector}. Position of the right variant.
#' \item{\code{rightVariant.chromosome}:} \emph{Character vector}. Chromosome of the right variant.
#' \item{\code{rightVariant.rsId}:} \emph{Character vector}. rsID of the right variant.
#' \item{\code{rightStudy.studyId}:} \emph{Character vector}. Study identifier for the right study.
#' \item{\code{rightStudy.traitReported}:} \emph{Character vector}. Reported trait associated with the colocalisation in the right study.
#' \item{\code{rightStudy.traitCategory}:} \emph{Character vector}. Category of the reported trait in the right study.
#' \item{\code{h3}:} \emph{Numeric vector}. H3 value associated with the colocalisation.
#' \item{\code{h4}:} \emph{Numeric vector}. H4 value associated with the colocalisation.
#' \item{\code{log2h4h3}:} \emph{Numeric vector}. Log2 ratio of H4 to H3 values associated with the colocalisation.
#' }
#' @examples
#' \dontrun{
#' result <- gwasColocalisationForRegion(chromosome = "1", start = 153992685, end = 154155116)
#' }
#' @import dplyr
#' @importFrom magrittr %>%
#' @export
#'
#' @references
#' - Giambartolomei, Claudia et al. “Bayesian test for colocalisation between pairs of
#' genetic association studies using summary statistics.” PLoS genetics vol. 10,5 e1004383. 15 May. 2014, doi:10.1371/journal.pgen.1004383


gwasColocalisationForRegion <- function(chromosome, start, end) {


  # Check if arguments are empty
  if (missing(chromosome) || is.null(chromosome) ||
      missing(start) || is.null(start) ||
      missing(end) || is.null(end)) {
    message("Please provide values for all the argument: chromosome, start, and end.")
    return(NULL)
  }
  ## Set up to query Open Targets Genetics API
  variables <- list(chromosome = chromosome, start = start, end = end)
  cli::cli_progress_step("Connecting the database...", spinner = TRUE)
  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()

  query <- "query gwasColForReg_query($chromosome: String!, $start: Long!, $end: Long!) {
    gwasColocalisationForRegion(chromosome: $chromosome, start: $start, end: $end) {
      leftVariant {
        id
        position
        chromosome
        rsId
      }
      leftStudy {
        studyId
        traitReported
        traitCategory
      }
      rightVariant {
        id
        position
        chromosome
        rsId
      }
      rightStudy {
        studyId
        traitReported
        traitCategory
      }
      h3
      h4
      log2h4h3
    }
  }"

  ## Execute the query
  otg_qry$query(name = "gwasColForReg_query", x = query)

  cli::cli_progress_step("Downloading the data...", spinner = TRUE)
  results <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$gwasColForReg_query, variables, flatten = TRUE))$data

  output <- results$gwasColocalisationForRegion %>% as.data.frame() %>% dplyr::tibble()

  return(output)
}

