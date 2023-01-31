#' Get gene information for an input ENSEMBL identifier
#'
#'
#' @param geneid An ENSEMBL identifier for a gene
#' @return A dataframe containing the input gene information like symbol, chromosome information, etc.
#' @examples
#' geneInfo("ENSG00000169174")
#' @export
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
  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$geneInfoquery, variables), flatten=TRUE)$data
  final_output <- as.data.frame(result$geneInfo)
  if (nrow(final_output)==0){
    final_output <- data.frame()
  }
  return (final_output)
}
