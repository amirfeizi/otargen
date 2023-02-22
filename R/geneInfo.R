#' Retrieves gene information for a gene ENSEMBL identifier.
#'
#' For a specified id, a table with the following columns
#' is generated- id, symbol, bioType, description, chromosome,
#' tss, start, end, fwdStrand, and exons.
#'
#' @param geneid String: a gene ENSEMBL identifier.
#'
#' @return Data frame containing the input gene information like symbol, chromosome information, etc.

#' @examples
#' geneInfo(geneid="ENSG00000169174")
#'
#' @export
#'
#'

geneInfo <- function(geneid) {

  ## Set up to query Open Targets Genetics API
  variables <- list(geneId = geneid)

  cli::cli_progress_step("Connecting the database...", spinner = TRUE)
  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()


  query <- "query geneInfoquery($geneId: String!){
  geneInfo(geneId:$geneId){
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

  otg_qry$query(name = "geneInfoquery", x =  query)

  cli::cli_progress_step("Downloading data...", spinner = TRUE)
  gene_info <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$geneInfoquery, variables), flatten=TRUE)$data
  final_output <- as.data.frame(gene_info$geneInfo)
  if (nrow(final_output)==0){
    final_output <- data.frame()
  }
  return (final_output)
}
