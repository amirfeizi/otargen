#' Retrieves GWAS colocalisation data.
#'
#'
#' Provided with study id and variant id, this function returns a tibble data table of all colocalizeed tag variants with the corresponding GWAS study information and colocalization scores.  A table is generated for which variants tag the input variant, the studies and statistics
#' The output table is expected to contain following data columns- indexVariant.id, indexVariant.position,
#' indexVariant.chromosome, indexVariant.rsId, study.studyId, study.traitReported,
#' study.traitCategory, beta, h3, h4 and log2h4h3.
#'
#'
#' @param studyid String: Open Target Genetics generated id for GWAS study.
#' @param variantid String: Open Target Genetics generated id for variant (CHR_POSITION_REFALLELE_ALT_ALLELE or rsId).
#'
#' @return Data frame of the studies which colocalise with the input variant and study.
#'
#' @examples
#' \dontrun{
#' otargen::gwasColocalisation(studyid = "GCST90002357", variantid = "1_154119580_C_A")
#' otargen::gwasColocalisation(studyid = "GCST90002357", variantid = "rs2494663")
#' }
#' @export
#'
#'

gwasColocalisation <- function(studyid, variantid) {
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


  query <- "query gwascolquery($studyId: String!, $variantId: String!){
    gwasColocalisation(studyId: $studyId, variantId: $variantId){
    indexVariant{
    id
    position
    chromosome
    rsId
    }
    study{
      studyId
      traitReported
      traitCategory
    }
    beta
    h3
    h4
    log2h4h3
  }
}"


  ## Execute the query
  output = data.frame()
  variables <- list(studyId = studyid, variantId = input_variantid)

  otg_qry$query(name = "gwascol_query", query)
  cli::cli_progress_step("Downloading data...", spinner = TRUE)
  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$gwascol_query, variables), flatten = TRUE)$data

  output <- result$gwasColocalisation %>% dplyr::tibble()

  if (nrow(output) !=0){
  output <- output[, c("study.studyId" ,"study.traitReported" , "study.traitCategory",
                       "indexVariant.id" , "indexVariant.position",
                       "indexVariant.chromosome", "indexVariant.rsId",
                       "beta", "h3" , "h4" ,"log2h4h3")]
  }
  return(output)
}
