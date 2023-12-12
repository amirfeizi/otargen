#' Retrieve GWAS summary statistics for a genomic region.
#'
#' For a given study ID and chromosomal region information, this function returns  data frame(tibble format) with all variants and their GWAS summary statistics.
#'
#' @param study_id Character: Open Target Genetics generated ID for the GWAS study.
#' @param chromosome Character: Chromosome number as a string.
#' @param start Integer: Start position of the specified chromosome.
#' @param end Integer: End position of the specified chromosome.
#'
#' @return Returns a data table of variant information and p-values with the following columns:
#' \itemize{
#'   \item{\code{variant.id}:} \emph{Character vector}. Variant identifier.
#'   \item{\code{variant.chromosome}:} \emph{Character vector}. Chromosome of the variant.
#'   \item{\code{variant.position}:} \emph{Integer vector}. Position of the variant.
#'   \item{\code{pval}:} \emph{Numeric vector}. P-value.
#' }
#'
#' @examples
#' \dontrun{
#' result <- gwasRegional(study_id = "GCST90002357",
#' chromosome = "1", start = 153992685, end = 154155116)
#' }
#' @import dplyr
#' @importFrom magrittr %>%
#' @export
#'
#'

gwasRegional <- function(study_id, chromosome, start, end) {

  ## Set up to query Open Targets Genetics API

  variables <- list(studyId = study_id, chromosome = chromosome, start = start, end = end)

tryCatch({
  cli::cli_progress_step("Connecting to the Open Targets Genetics GrpahQL API...", spinner = TRUE)
  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()

  query <- "query gwasregionalquery($studyId: String!, $chromosome: String!, $start: Long!, $end: Long!) {
    gwasRegional(studyId: $studyId, chromosome: $chromosome, start: $start, end: $end) {
      variant {
        id
        chromosome
        position
      }
      pval
    }
  }"

  ## Execute the query

  otg_qry$query(name = "gwasregional_query", x = query)

  cli::cli_progress_step("Downloading data...", spinner = TRUE)
  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$gwasregional_query, variables), flatten = TRUE)$data

  output <- result$gwasRegional %>%
    dplyr::select(variant.id, variant.chromosome, variant.position, pval) %>%
    dplyr::as_tibble()

  if (nrow(output) == 0) {
    output <- data.frame()
  }

  return(output)

}, error = function(e) {
  # Handling connection timeout
  if(grepl("Timeout was reached", e$message)) {
    stop("Connection timeout reached while connecting to the Open Targets Genetics GraphQL API.")
  } else {
    stop(e) # Handle other types of errors
  }
})

}
