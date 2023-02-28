#' Retrieves GWAS colocalisation data for a region.
#'
#' With providing a defined chromosomal right this function reruns a tibble data table
#' of colocalized variants to the left and right side of the provided regions including
#' the calculated colocalization scores. The following data columns is expected for the output-
#' leftVariant.id, leftVariant.position, leftVariant.chromosome,
#' leftVariant.rsId, leftStudy.studyId, leftStudy.traitReported,
#' leftStudy.traitCategory, rightVariant.id, rightVariant.position,
#' rightVariant.chromosome, rightVariant.rsId, rightStudy.studyId,
#' rightStudy.traitReported, rightStudy.traitCategory, h3, h4
#' and log2h4h3.
#'
#' @param chromosome String: chromosome number as string.
#' @param start Long: start position of the specified chromosome.
#' @param end Long: end position of the specified chromosome.
#'
#'
#' @returns Data frame with GWAS colocalisation data for a specified region.
#'
#' @examples
#' gwasColocalisationForRegion(chromosome = "1", start = 153992685, end = 154155116)
#'
#' @export
#'
#'

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
