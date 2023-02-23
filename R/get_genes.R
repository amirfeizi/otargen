#' Retrieves information about genes on an input locus
#'
#' A table is generated containing information about all the genes
#' present in the specified locus. The columns are id, symbol, bioType,
#' description, chromosome, tss, start, end, fwdStrand, and exons.
#'
#' @param chromosome String: chromosome number as string.
#' @param start Long: start position of the specified chromosome.
#' @param end Long: end position of the specified chromosome.
#'
#' @return Data frame containing the details of all the genes in the mentioned locus.
#'
#' @examples
#' get_genes(chromosome = "1", start = 800, end = 500000)
#'
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
  final_output <- as.data.frame(genes_result$genes)
  if (nrow(final_output) == 0) {
    final_output <- data.frame()
  }
  return(final_output)
}
