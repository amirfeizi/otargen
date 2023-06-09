#' Get GWAS credible set data for a variant in a study
#'
#' Provided with study id and a lead variant id parameters,
#' this functions returns a data table consisting of all the associated credible
#' set tag variants with the corresponding statistical data.
#'
#' @param studyid String: Open Target Genetics generated id for GWAS study.
#' @param variantid String: Open Target Genetics generated id for variant (CHR_POSITION_REFALLELE_ALT_ALLELE or rsId).
#'
#' @return Returns a data frame of results from credible set of variants for a specific lead variant with the following columns:
#'
#' \enumerate{
#' \item tagVariant.id
#' \item tagVariant.rsId
#' \item beta
#' \item postProb
#' \item pval
#' \item se
#' \item MultisignalMethod
#' \item logABF
#' \item is95
#' \item is99
#' }
#'
#' @examples
#' \dontrun{
#' otargen::gwasCredibleSet(studyid="GCST90002357", variantid="1_154119580_C_A")
#' otargen::gwasCredibleSet(studyid="GCST90002357", variantid="rs2494663")
#' }
#' @import dplyr
#' @importFrom magrittr %>%
#' @export
#'

gwasCredibleSet <- function(studyid, variantid) {


  ## Query for GWAS study locus details


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

  query <- "query credsetQuery($studyId: String!, $variantId: String!){
  gwasCredibleSet(studyId: $studyId, variantId: $variantId) {
    tagVariant {
      id
      rsId
    }
    beta
    postProb
    pval
    se
    MultisignalMethod
    logABF
    is95
    is99
  }
}"

  ## Execute the query

  output <- data.frame()
  variables <- list(studyId = studyid, variantId = input_variantid)

  otg_qry$query(name = "credset_query", x =  query)

  cli::cli_progress_step("Downloading the data...", spinner = TRUE)
  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$credset_query,
                                            variables, flatten = TRUE))$data


  output <- result$gwasCredibleSet %>% as.data.frame()

  return(output)

}
