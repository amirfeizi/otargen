#' Retrieves the information about the input variant id.
#'
#' For an input variant id, a table is generated with the following columns -
#' chromosome, position, refAllele, altAllele, rsId, chromosomeB37 , positionB37, id,
#' nearestGene.id, nearestGene.symbol, nearestGeneDistance, nearestCodingGene.id, nearestCodingGene.symbol,
#' nearestCodingGeneDistance, mostSevereConsequence, caddRaw, caddPhred, gnomadAFR,	gnomadAMR,	gnomadASJ,
#' gnomadEAS, gnomadFIN,	gnomadNFE,	gnomadNFEEST,	gnomadNFENWE,	gnomadNFESEU,	gnomadNFEONF, and	gnomadOTH.
#'
#' @param variantid String: Open Target Genetics generated id for variant (CHR_POSITION_REFALLELE_ALT_ALLELE or rsId).
#'
#' @return Data frame containing the variant information with the above mentioned columns.
#'
#' @examples
#' var_info <- variantInfo(variantid = "1_55039974_G_T")
#' or
#' var_info <- variantInfo(variantid = "rs11591147")
#'
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
