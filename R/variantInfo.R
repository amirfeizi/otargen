#' Retrieves the information about the input variant ID.
#'
#' @param \emph{variantid} String: Open Targets Genetics generated ID for a variant (CHRPOSITION_REFALLELE_ALTALLELE or rsId).
#'
#' @return Returns a data frame containing the variant information with the following columns:
#' \itemize{
#'   \item{\code{chromosome}:} \emph{Character}. Chromosome of the variant.
#'   \item{\code{position}:} \emph{Integer}. Position of the variant.
#'   \item{\code{refAllele}:} \emph{Character}. Reference allele of the variant.
#'   \item{\code{altAllele}:} \emph{Character}. Alternate allele of the variant.
#'   \item{\code{rsId}:} \emph{Character}. rsID of the variant.
#'   \item{\code{chromosomeB37}:} \emph{Character}. Chromosome of the variant in build 37.
#'   \item{\code{positionB37}:} \emph{Integer}. Position of the variant in build 37.
#'   \item{\code{id}:} \emph{Character}. ID of the variant.
#'   \item{\code{nearestGene.id}:} \emph{Character}. ID of the nearest gene to the variant.
#'   \item{\code{nearestGene.symbol}:} \emph{Character}. Symbol of the nearest gene to the variant.
#'   \item{\code{nearestGeneDistance}:} \emph{Integer}. Distance between the variant and the nearest gene.
#'   \item{\code{nearestCodingGene.id}:} \emph{Character}. ID of the nearest coding gene to the variant.
#'   \item{\code{nearestCodingGene.symbol}:} \emph{Character}. Symbol of the nearest coding gene to the variant.
#'   \item{\code{nearestCodingGeneDistance}:} \emph{Integer}. Distance between the variant and the nearest coding gene.
#'   \item{\code{mostSevereConsequence}:} \emph{Character}. Most severe consequence of the variant.
#'   \item{\code{caddRaw}:} \emph{Numeric}. CADD raw score of the variant.
#'   \item{\code{caddPhred}:} \emph{Numeric}. CADD Phred score of the variant.
#'   \item{\code{gnomadAFR}:} \emph{Numeric}. gnomAD allele frequency in African/African American population.
#'   \item{\code{gnomadAMR}:} \emph{Numeric}. gnomAD allele frequency in Latino/Admixed American population.
#'   \item{\code{gnomadASJ}:} \emph{Numeric}. gnomAD allele frequency in Ashkenazi Jewish population.
#'   \item{\code{gnomadEAS}:} \emph{Integer}. gnomAD allele frequency in East Asian population.
#'   \item{\code{gnomadFIN}:} \emph{Numeric}. gnomAD allele frequency in Finnish population.
#'   \item{\code{gnomadNFE}:} \emph{Numeric}. gnomAD allele frequency in Non-Finnish European population.
#'   \item{\code{gnomadNFEEST}:} \emph{Numeric}. gnomAD allele frequency in Non-Finnish European (Estonian) population.
#'   \item{\code{gnomadNFENWE}:} \emph{Numeric}. gnomAD allele frequency in Non-Finnish European (Northwest European) population.
#' }
#'
#' @examples
#' \dontrun{
#' result <- variantInfo(variantid = "1_55039974_G_T")
#' result <- variantInfo(variantid = "rs11591147")
#'}
#' @importFrom magrittr %>%
#' @import dplyr
#' @import tidyr
#' @export
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
