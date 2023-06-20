#' Retrieve gene summary information.
#'
#' This function takes a gene ENSEMBL id (e.g.ENSG00000169174) or a gene name (e.g.PCSK9) and returns a table containing details about
#' input genes , such as genomic location, gene structure and its BioType.
#'
#' @param gene Character: an ENSEMBL gene identifier (e.g.ENSG00000169174) or gene name (e.g. PCSK9).
#'
#' @return Returns a data frame (tibble format) with the following data structure:
#'
#' \enumerate{
#' \item id
#' \item symbol
#' \item bioType
#' \item description
#' \item chromosome
#' \item tss
#' \item start
#' \item end
#' \item fwdStrand
#' \item exons
#' }

#' @examples
#' \dontrun{
#' result <- geneInfo(gene="ENSG00000169174")
#' result <- geneInfo(gene="PCSK9")
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

  # Query for gene name search
  query_search <- "query convertnametoid($queryString:String!) {
    search(queryString:$queryString){
      genes{
        id
        symbol
      }
    }
  }"

  # Check gene format
  if (!grepl(pattern = "ENSG\\d{11}", gene)) {
    variables <- list(queryString = gene)
    otg_qry$query(name = "convertnametoid", x = query_search)
    id_result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$convertnametoid, variables), flatten = TRUE)$data
    id <- as.data.frame(id_result$search$genes)
    if (nrow(id) != 0) {
      name_match <- id[id$symbol == gene, ]
      gene_input <- name_match$id
    } else {
      stop ("\nPlease provide Ensemble gene ID or gene name")
    }
  } else {
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
  # Executing the query
  gene_info <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$geneInfoQuery, variables), flatten = TRUE)$data

  output <- gene_info$geneInfo %>% as.data.frame() %>%
    dplyr::distinct(symbol, .keep_all = TRUE) %>%
    dplyr::tibble() # Converting to tibble format

  if (nrow(output) == 0) {
    output <- data.frame()
  }
  return(output)
}

