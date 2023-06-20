#' Retrieve summary information of the genes in a locus
#'
#' This function retrieves information for a specified region on a chromosome, including overlapping genes and their
#' associated details such as ID, symbol, biotype, description,
#'  transcription start site (TSS), start position, end position, strand direction, and exons structure.
#'
#' @param chromosome Character: Chromosome number as a string.
#' @param start Integer: Start position of the specified region on the chromosome.
#' @param end Integer: End position of the specified region on the chromosome.
#'
#' @return Returns a tibble data frame of all the overlapping genes in the specified region with the following data structure:
#' \itemize{
#'   \item{\code{id}:} \emph{Character}. ID of the gene.
#'   \item{\code{symbol}:} \emph{Character}. Symbol of the gene.
#'   \item{\code{bioType}:} \emph{Character}. Biotype of the gene.
#'   \item{\code{description}:} \emph{Character}. Description of the gene.
#'   \item{\code{chromosome}:} \emph{Character}. Chromosome of the gene.
#'   \item{\code{tss}:} \emph{Integer}. Transcription start site of the gene.
#'   \item{\code{start}:} \emph{Integer}. Start position of the gene.
#'   \item{\code{end}:} \emph{Integer}. End position of the gene.
#'   \item{\code{fwdStrand}:} \emph{Logical}. Strand direction of the gene.
#'   \item{\code{exons}:} \emph{List}. List of exons of the gene.
#' }
#'
#' @examples
#' \dontrun{
#' result <- get_genes(chromosome = "2", start = 239634984, end = 241634984)
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
