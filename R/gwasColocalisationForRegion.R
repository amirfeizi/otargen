#' Retrieves GWAS colocalisation data for a region.
#'
#' Given a defined chromosomal region, this function returns a tibble data table
#' of colocalised variants to the left and right side of the provided region, including
#' the calculated colocalization scores.
#'
#' @param chromosome String: Chromosome number as a string.
#' @param start Long: Start position of the specified chromosome.
#' @param end Long: End position of the specified chromosome.
#'
#' @return Returns a data frame with the following columns:
#' \itemize{
#'   \item{\code{study.studyId}:} \emph{Character vector}. Study identifier.
#'   \item{\code{study.traitReported}:} \emph{Character vector}. Reported trait associated with the colocalisation.
#'   \item{\code{study.traitCategory}:} \emph{Character vector}. Category of the reported trait.
#'   \item{\code{indexVariant.id}:} \emph{Character vector}. ID of the index variant.
#'   \item{\code{indexVariant.position}:} \emph{Integer vector}. Position of the index variant.
#'   \item{\code{indexVariant.chromosome}:} \emph{Character vector}. Chromosome of the index variant.
#'   \item{\code{indexVariant.rsId}:} \emph{Character vector}. rsID of the index variant.
#'   \item{\code{beta}:} \emph{Numeric vector}. Beta value associated with the colocalisation.
#'   \item{\code{h3}:} \emph{Numeric vector}. H3 value associated with the colocalisation.
#'   \item{\code{h4}:} \emph{Numeric vector}. H4 value associated with the colocalisation.
#'   \item{\code{log2h4h3}:} \emph{Numeric vector}. Log2 ratio of H4 to H3 values.
#' }
#'
#' @examples
#' \dontrun{
#' gwasColocalisationForRegion(chromosome = "1", start = 153992685, end = 154155116)
#' }
#' @import dplyr
#' @importFrom magrittr %>%
#' @export
#'
#' @references
#' - Giambartolomei, Claudia et al. “Bayesian test for colocalisation between pairs of
#' genetic association studies using summary statistics.” PLoS genetics vol. 10,5 e1004383. 15 May. 2014, doi:10.1371/journal.pgen.1004383

gwasColocalisationForRegion <- function(chromosome, start, end) {
  ## Set up to query Open Targets Genetics API
  variables <- list(chromosome = chromosome, start = start, end = end)

  cli::cli_progress_step("Connecting the dataase...", spinner = TRUE)
  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()

  query <- "query gwasColForReg_query($chromosome: String!, $start: Long!, $end: Long!){
  gwasColocalisationForRegion(chromosome: $chromosome, start: $start, end: $end) {
    leftVariant{
      id
    position
    chromosome
      rsId
    }
  leftStudy{
    studyId
    traitReported
    traitCategory
  }
  rightVariant
  {
    id
    position
    chromosome
    rsId
  }
  rightStudy
  {
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
