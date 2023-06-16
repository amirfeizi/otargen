#' Retrieves colocalisation data for a variant in a study
#'
#' Fetches various colocalisation data on molecular quantitative trait locus
#' (QTL) analysis for a variant in a specific study.
#'
#' @param \emph{studyid} String: Open Targets Genetics generated ID for GWAS studies.
#' @param \emph{variantid} String: Open Targets Genetics generated ID for variant (CHRPOSITION_REFALLELE_ALTALLELE or rsId).
#'
#' @return Returns a data frame of the colocalisation information for a lead variant in a specific study.
#' The table consists of the following columns:
#' \itemize{
#'   \item{\code{qtlStudyName}:} \emph{Character vector}. QTL study name.
#'   \item{\code{phenotypeId}:} \emph{Character vector}. Phenotype ID.
#'   \item{\code{gene.id}:} \emph{Character vector}. Gene ID.
#'   \item{\code{gene.symbol}:} \emph{Character vector}. Gene symbol.
#'   \item{\code{name}:} \emph{Character vector}. Tissue name.
#'   \item{\code{indexVariant.id}:} \emph{Character vector}. Index variant ID.
#'   \item{\code{indexVariant.rsId}:} \emph{Character vector}. Index variant rsID.
#'   \item{\code{beta}:} \emph{Numeric}. Beta value.
#'   \item{\code{h4}:} \emph{Numeric}. h4 value.
#'   \item{\code{h3}:} \emph{Numeric}. h3 value.
#'   \item{\code{log2h4h3}:} \emph{Numeric}. Log2(h4/h3) value.
#' }
#'
#' @examples
#' \dontrun{
#' result <- qtlColocalisationVariantQuery(studyid = "GCST90002357", variantid = "1_154119580_C_A")
#' result <- qtlColocalisationVariantQuery(studyid = "GCST90002357", variantid = "rs2494663")
#' }
#' @import dplyr
#' @import cli
#' @import ghql
#' @importFrom magrittr %>%
#' @export
#'
#'


qtlColocalisationVariantQuery <- function(studyid, variantid) {

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


  ## Query for QTL colocalisation

  query <- "query qtlColocalisationVariantQuery($studyId: String!, $variantId: String!) {
  qtlColocalisation(studyId: $studyId, variantId: $variantId){
    qtlStudyName
    phenotypeId
    gene {
      id
      symbol
    }
    tissue {
      name
    }
    indexVariant {
      id
      rsId
    }
    beta
    h4
    h3
    log2h4h3
  }
}"

  # execute the query

  variables <- list(studyId = studyid, variantId = input_variantid)


  otg_qry$query(name = "qtl_query", x = query)

  cli::cli_progress_step("Downloading data...", spinner = TRUE)
  qtlcoloc_result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$qtl_query, variables, flatten = TRUE))$data
  df_qtlcoloc <- as.data.frame(qtlcoloc_result$qtlColocalisation)
  return(df_qtlcoloc)
}
