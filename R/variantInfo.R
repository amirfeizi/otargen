#' Gets the information about the input variant id.
#'
#'
#' @param variantid is the Open Target Genetics generated id for each variant in the database.
#' @return A dataframe containing the variant information.
#' @examples
#' variantInfo("1_55039974_G_T")
#' variantInfo("rs4129267")
#' @export
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
    id_result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$convertid, variables), flatten=TRUE)$data
    input_variantid <- id_result$search$variants$id
  }

  else if (grepl(pattern = "\\d+_\\d+_[a-zA-Z]+_[a-zA-Z]+", variantid))
  {
    input_variantid <- variantid
  }
  else
  {
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

  otg_qry$query(name = "variantInfoquery", x =  query)

  cli::cli_progress_step("Downloading data...", spinner = TRUE)
  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$variantInfoquery, variables), flatten = TRUE)$data
  result <- lapply(result$variantInfo, function(x) if (is.null(x)) "NA" else x)
  result <- as.data.frame(result)
  return (result)
}
