#' Retrieves gene information for a gene ENSEMBL identifier.
#'
#' This function takes gene ensembl id (e.g.ENSG00000169174) and it returns tibble data table with basic annotation with following columns
#' - id, symbol, bioType, description, chromosome,
#' tss, start, end, fwdStrand, and exons.
#'
#' @param geneid String: a ensembl gene identifier (e.g.ENSG00000169174).
#'
#' @return tibble data table containing the input gene information such as symbol, chromosome information, etc.

#' @examples
#' \dontrun{
#' otargen::geneInfo(geneid = "ENSG00000169174")
#' }
#' @export
#'
#'

geneInfo <- function(geneid) {
  ## Set up to query Open Targets Genetics API
  variables <- list(geneId = geneid)

  cli::cli_progress_step("Connecting the database...", spinner = TRUE)
  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()


  query <- "query geneInfoQuery($geneId: String!){
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

  otg_qry$query(name = "geneInfoQuery", x = query)

  cli::cli_progress_step("Downloading data...", spinner = TRUE)
  # executing the query
  gene_info <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$geneInfoQuery, variables), flatten = TRUE)$data

  output <- gene_info$geneInfo %>% as.data.frame() %>%
    dplyr::distinct(symbol, .keep_all = TRUE) %>% dplyr::tibble() # converting to tibble format

  if (nrow(output) == 0) {
    output <- data.frame()
  }
  return(output)
}
