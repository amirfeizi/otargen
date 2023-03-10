#' Retrieves gene information for a gene ENSEMBL identifier or a gene name.
#'
#' This function takes gene ensembl id (e.g.ENSG00000169174) or gene name (e.g.PCSK9) and it returns tibble data table with basic annotation with following columns
#' - id, symbol, bioType, description, chromosome,
#' tss, start, end, fwdStrand, and exons.
#'
#' @param gene String: a ensembl gene identifier or gene name (e.g.ENSG00000169174/PCSK9).
#'
#' @return tibble data table containing the input gene information such as symbol, chromosome information, etc.

#' @examples
#' \dontrun{
#' otargen::geneInfo(gene="ENSG00000169174")
#' otargen::geneInfo(gene="PCSK9")
#' }
#' @import dplyr
#' @importFrom magrittr %>%
#' @export
#'
#'

geneInfo <- function(gene) {
  ## Set up to query Open Targets Genetics API

  cli::cli_progress_step("Connecting the database...", spinner = TRUE)
  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()

  #Query for gene name search
  # check for gene name:
  query_search <- "query convertnametoid($queryString:String!) {
    search(queryString:$queryString){
      genes{
        id
        symbol
      }
      }
    }"

  if (!grepl(pattern = "ENSG\\d{11}", gene)){
    variables <- list(queryString = gene)
    otg_qry$query(name = "convertnametoid", x = query_search)
    id_result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$convertnametoid, variables), flatten = TRUE)$data
    id <- as.data.frame(id_result$search$genes)
    if (nrow(id)!=0){
      name_match <- id[id$symbol == gene, ]
      gene_input <- name_match$id
    } else{
      stop ("\nPlease provide Ensemble gene ID or gene name")
    }

  } else{
   gene_input <- gene
  }

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


  variables <- list(geneId = gene_input)

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
