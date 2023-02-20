#' Information about genes on an input locus
#'
#'
#' @param chromosome Chromosome given as a string
#' @param start start position of the input chromosome
#' @param end end position of the input chromosome
#' @return A dataframe containing the details of all the genes in the mentioned locus.
#' @examples
#' genes(chromosome="1", start=800, end=500000)
#' @export
#'

genes <- function(chromosome, start, end) {

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

  otg_qry$query(name = "genesquery", x =  query)
  cli::cli_progress_step("Downloading data...", spinner = TRUE)
  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$genesquery, variables), flatten=TRUE)$data
  final_output <- as.data.frame(result$genes)
  if (nrow(final_output)==0){
    final_output <- data.frame()
  }
  return (final_output)
}
