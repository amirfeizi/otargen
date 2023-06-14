#' Retrieves the information about the input variant id.
#'
#' @param \emph{variantid} String: Open Target Genetics generated id for variant (CHRPOSITION_REFALLELE_ALTALLELE or rsId).
#'
#' @return Returns a data frame containing the variant information with the following columns:
#'
#' \enumerate{
#' \item chromosome
#' \item position
#' \item refAllele
#' \item altAllele
#' \item rsId
#' \item chromosomeB37
#' \item positionB37
#' \item id
#' \item nearestGene.id
#' \item nearestGene.symbol
#' \item nearestGeneDistance
#' \item nearestCodingGene.id
#' \item nearestCodingGene.symbol
#' \item nearestCodingGeneDistance
#' \item mostSevereConsequence
#' \item caddRaw
#' \item caddPhred
#' \item gnomadAFR
#' \item gnomadAMR
#' \item gnomadASJ
#' \item gnomadEAS
#' \item gnomadFIN
#' \item gnomadNFE
#' \item gnomadNFEEST
#' \item gnomadNFENWE
#' }
#'
#' @examples
#' \dontrun{
#' otargen::variantInfo(variantid = "1_55039974_G_T")
#' otargen::variantInfo(variantid = "rs11591147")
#'}
#' @importFrom magrittr %>%
#' @import dplyr
#' @import tidyr
#' @export
#'
#'
#'
variantInfo <- function(variantid) {
  ## Set up to query Open Targets Genetics API


  cli::cli_progress_step("Connecting the database...", spinner = TRUE)
  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()

  # Check variant id format
  if (grepl(pattern = "rs\\d+", variantid)) {
    # Convert rs id to variant id
    query_searchid <- "query ConvertRSIDtoVID($queryString:String!) {
    search(queryString:$queryString){
      totalVariants
      variants{
        id
        }
      }
    }"

    variables <- list(queryString = variantid)
    otg_qry$query(name = "convertid", x = query_searchid)
    id_result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$convertid, variables), flatten = TRUE)$data
    input_variantid <- id_result$search$variants$id
  } else if (grepl(pattern = "\\d+_\\d+_[a-zA-Z]+_[a-zA-Z]+", variantid)) {
    input_variantid <- variantid
  } else {
    stop("\n Please provide a variant Id")
  }


  query <- "query variantInfoquery($variantId: String!){
  variantInfo(variantId: $variantId){
    chromosome
    position
    refAllele
    altAllele
    rsId
    chromosomeB37
    positionB37
    id
    nearestGene{
      id
      symbol
    }
    nearestGeneDistance
    nearestCodingGene{
      id
      symbol
    }
    nearestCodingGeneDistance
    mostSevereConsequence
    caddRaw
    caddPhred
    gnomadAFR
    gnomadAMR
    gnomadASJ
    gnomadEAS
    gnomadFIN
    gnomadNFE
    gnomadNFEEST
    gnomadNFENWE
    gnomadNFESEU
    gnomadNFEONF
    gnomadOTH
  }
  }"


  ## Execute the query

  variables <- list(variantId = input_variantid)

  otg_qry$query(name = "variantInfoquery", x = query)

  cli::cli_progress_step("Downloading data...", spinner = TRUE)
  var_info <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$variantInfoquery, variables), flatten = TRUE)$data
  var_info <- as.data.frame(var_info$variantInfo)
  return(var_info)
}
