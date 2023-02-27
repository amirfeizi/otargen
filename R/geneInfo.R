#' Retrieves gene information for a gene ENSEMBL identifier.
#'
#' For a specified id, a table with the following columns
#' is generated- id, symbol, bioType, description, chromosome,
#' tss, start, end, fwdStrand, and exons.
#'
#' @param geneid String: a ensembl gene identifier (e.g.ENSG00000169174).
#'
#' @return tibble data table containing the input gene information such as symbol, chromosome information, etc.

#' @examples
#' geneInfo(geneid = "ENSG00000169174")
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

  otg_qry$query(name = "geneInfoquery", x = query)

  cli::cli_progress_step("Downloading data...", spinner = TRUE)
  gene_info <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$geneInfoquery, variables), flatten = TRUE)$data
  output <- gene_info$geneInfo %>% dplyr::tibble() # converting to tibble format

  if (nrow(output) == 0) {
    output <- data.frame()
  }
  return(output)
}
