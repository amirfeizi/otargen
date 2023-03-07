#' Retrieves information about genes on an input locus
#'
#' This function gets information for a region of a specified chromosome and returns a tibble data table of all the overlapping genes in the specified region with following information columns: id, symbol, bioType,
#' description, chromosome, tss, start, end, fwdStrand, and exons.
#'
#' @param chromosome String: chromosome number as string.
#' @param start Long: start position of the specified chromosome.
#' @param end Long: end position of the specified chromosome.
#'
#' @return tibble data table containing the details of all the genes in the mentioned locus.
#'
#' @examples
#' \dontrun{
#' otargen::get_genes(chromosome = "2", start = 239634984, end = 241634984)
#' }
#' @import dplyr
#' @importFrom magrittr %>%
#' @export
#'
#'
get_genes <- function(chromosome, start, end) {
  ## Set up to query Open Targets Genetics API
  variables <- list(chromosome = chromosome, start = start, end = end)

  cli::cli_progress_step("Connecting the database...", spinner = TRUE)
  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()


  query <- "query genesquery($chromosome: String! $start: Long!, $end: Long!){
  genes(chromosome: $chromosome, start: $start, end: $end){
    id
    symbol
    bioType
    description
    chromosome
    tss
    start
    end
    fwdStrand
    exons
  }
  }"


  ## Execute the query

  otg_qry$query(name = "genesquery", x = query)
  cli::cli_progress_step("Downloading data...", spinner = TRUE)

  genes_result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$genesquery, variables), flatten = TRUE)$data
  output <- as.data.frame(genes_result$genes) %>% dplyr::tibble()

  if (nrow(output) == 0) {
    final_output <- data.frame()
  }
  return(output)
}
