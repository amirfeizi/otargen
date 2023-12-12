#' Retrieves information about a variant.
#'
#' For a given variant ID, this function retrieves information about the variant, including its chromosome,
#' position, reference allele, alternative allele, rsID, nearest gene, most severe consequence, and allele
#' frequencies in different populations from gnomAD databse. The Genome Aggregation Database (gnomAD) is a
#' resource developed by an international coalition of investigators, with the goal of aggregating and
#' harmonizing both exome and genome sequencing data from a wide variety of large-scale sequencing projects,
#' and making summary data available for the wider scientific community (see the reference).
#'
#' @param variant_id Character: generated ID for variants by Open Targets Genetics (e.g. 1_154119580_C_A) or rsId (rs2494663).
#'
#' @return Returns a data frame (in tibble format) containing information about the variant.
#' The data frame has the following structure:
#' \itemize{
#'   \item{\code{chromosome}:} \emph{Character}. Chromosome of the variant.
#'   \item{\code{position}:} \emph{Integer}. Position of the variant.
#'   \item{\code{refAllele}:} \emph{Character}. Reference allele.
#'   \item{\code{altAllele}:} \emph{Character}. Alternative allele.
#'   \item{\code{rsId}:} \emph{Character}. Variant rsID.
#'   \item{\code{chromosomeB37}:} \emph{Character}. Chromosome of the variant in build 37 coordinates.
#'   \item{\code{positionB37}:} \emph{Integer}. Position of the variant in build 37 coordinates.
#'   \item{\code{id}:} \emph{Character}. Variant ID.
#'   \item{\code{nearestGene.id}:} \emph{Character}. ID of the nearest gene to the variant.
#'   \item{\code{nearestGene.symbol}:} \emph{Character}. Symbol of the nearest gene to the variant.
#'   \item{\code{nearestGeneDistance}:} \emph{Integer}. Distance to the nearest gene.
#'   \item{\code{nearestCodingGene.id}:} \emph{Character}. ID of the nearest coding gene to the variant.
#'   \item{\code{nearestCodingGene.symbol}:} \emph{Character}. Symbol of the nearest coding gene to the variant.
#'   \item{\code{nearestCodingGeneDistance}:} \emph{Integer}. Distance to the nearest coding gene.
#'   \item{\code{mostSevereConsequence}:} \emph{Character}. Most severe consequence of the variant.
#'   \item{\code{caddRaw}:} \emph{Numeric}. CADD raw score.
#'   \item{\code{caddPhred}:} \emph{Numeric}. CADD phred score.
#'   \item{\code{gnomadAFR}:} \emph{Numeric}. Allele frequency in the African/African-American population in gnomAD.
#'   \item{\code{gnomadAMR}:} \emph{Numeric}. Allele frequency in the Latino/Admixed American population in gnomAD.
#'   \item{\code{gnomadASJ}:} \emph{Numeric}. Allele frequency in the Ashkenazi Jewish population in gnomAD.
#'   \item{\code{gnomadEAS}:} \emph{Numeric}. Allele frequency in the East Asian population in gnomAD.
#'   \item{\code{gnomadFIN}:} \emph{Numeric}. Allele frequency in the Finnish population in gnomAD.
#'   \item{\code{gnomadNFE}:} \emph{Numeric}. Allele frequency in the Non-Finnish European population in gnomAD.
#'   \item{\code{gnomadNFEEST}:} \emph{Numeric}. Allele frequency in the Estonian population in gnomAD.
#'   \item{\code{gnomadNFENWE}:} \emph{Numeric}. Allele frequency in the Northwest European population in gnomAD.
#'   \item{\code{gnomadNFESEU}:} \emph{Numeric}. Allele frequency in the Southern European population in gnomAD.
#'   \item{\code{gnomadNFEONF}:} \emph{Numeric}. Allele frequency in the Other Non-Finnish European population in gnomAD.
#'   \item{\code{gnomadOTH}:} \emph{Numeric}. Allele frequency in other populations in gnomAD.
#' }
#'
#' @examples
#' \dontrun{
#' result <- variantInfo(variant_id = "rs2494663")
#'}
#'
#'@references https://gnomad.broadinstitute.org/
#' @importFrom magrittr %>%
#' @export
#'

variantInfo <- function(variant_id) {
  # Check if the variant ID argument is empty or null
  if (missing(variant_id) || is.null(variant_id) || variant_id == "") {
    message("Please provide a value for the variant ID argument.")
    return(NULL)
  }

  # Try-catch block for handling connection timeout
  tryCatch({
    # Set up to query Open Targets Genetics API
    cli::cli_progress_step("Connecting to the Open Targets Genetics GraphQL API...", spinner = TRUE)
    otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
    otg_qry <- ghql::Query$new()

    # Check variant id format
    if (grepl(pattern = "rs\\d+", variant_id)) {
      # Convert rs id to variant id
      query_searchid <- "query rsi2vid($queryString:String!) {
      search(queryString:$queryString){
        totalVariants
        variants{
          id
          }
        }
      }"

      variables <- list(queryString = variant_id)
      otg_qry$query(name = "rsi2vid", x = query_searchid)
      id_result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$rsi2vid, variables), flatten = TRUE)$data
      input_variant_id <- id_result$search$variants$id
    } else if (grepl(pattern = "\\d+_\\d+_[a-zA-Z]+_[a-zA-Z]+", variant_id)) {
      input_variant_id <- variant_id
    } else {
      stop("\nPlease provide a variant ID")
    }

    # Check if the input_variant_id is null or empty
    if (is.null(input_variant_id) || input_variant_id == "") {
      stop("There is no variant ID defined for this rsID by Open Target Genetics")
    }

  # Define the query
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

  # Execute the query
  variables <- list(variantId = input_variant_id)
  otg_qry$query(name = "variantInfoquery", x = query)
  cli::cli_progress_step("Downloading data...", spinner = TRUE)
  var_info <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$variantInfoquery, variables), flatten = TRUE)$data

  # Flatten and return data frame
  flat_var_info <- unlist(var_info$variantInfo)
  df_var_info <- as.data.frame(t(flat_var_info))
  names(df_var_info) <- names(flat_var_info)
  return(df_var_info)

  }, error = function(e) {
    # Handling connection timeout
    if(grepl("Timeout was reached", e$message)) {
      stop("Connection timeout reached while connecting to the Open Targets Genetics GraphQL API.")
    } else {
      stop(e) # Handle other types of errors
    }
  })
}
