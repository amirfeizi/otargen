#' Retrieves index variants and studies for a tag variant.
#'
#' For an input tag variant id, this function returns a  data table in tibble format
#' with population level summary stats data columns across various GWAS studies.
#' The following columns are expected in the output table:
#' indexVariant, study, pval, pvalMantissa, pvalExponent, nTotal, nCases, overallR2,
#' afr1000GProp, amr1000GProp, eas1000GProp, eur1000GProp, sas1000GProp, log10Abf,
#' posteriorProbability, oddsRatio, oddsRatioCILower, oddsRatioCIUpper, beta,
#' betaCILower, betaCIUpper, direction.
#'
#'
#' @param variantid String: Open Target Genetics generated id for variant (CHR_POSITION_REFALLELE_ALT_ALLELE or rsId).
#' @param pageindex Int: Index of the current page, pagination index >= 0.
#' @param pagesize Int: No. of records in a page, pagination size > 0.
#'
#' @return Dataframe containing the variant associated to the input tag variant.
#'
#' @examples
#' \dontrun{
#' indexVariantsAndStudiesForTagVariant(variantid = "1_109274968_G_T")
#' indexVariantsAndStudiesForTagVariant(variantid = "rs12740374",
#'  pageindex = 1, pagesize = 50)
#' }
#' @import dplyr
#' @importFrom magrittr %>%
#' @export
#'
#'

indexVariantsAndStudiesForTagVariant <- function(variantid, pageindex = 0, pagesize = 20) {
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


  query <- "query indexvariantsandstudiesquery($variantId: String!, $pageIndex: Int!, $pageSize:Int!){
    indexVariantsAndStudiesForTagVariant(variantId: $variantId, pageIndex: $pageIndex, pageSize: $pageSize){
    associations{
      indexVariant{
        id
        rsId
      }
      study{
        studyId
        traitReported
        traitCategory
      }
      pval
      pvalMantissa
      pvalExponent
      nTotal
      nCases
      overallR2
      afr1000GProp
      amr1000GProp
      eas1000GProp
      eur1000GProp
      sas1000GProp
      log10Abf
      posteriorProbability
      oddsRatio
      oddsRatioCILower
      oddsRatioCIUpper
      beta
      betaCILower
      betaCIUpper
      direction
    }
  }
}"

  ## Execute the query
  variables <- list(variantId = input_variantid, pageIndex = pageindex, pageSize = pagesize)

  otg_qry$query(name = "indexvariantsandstudiesquery", x = query)

  cli::cli_progress_step("Downloading data...", spinner = TRUE)
  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$indexvariantsandstudiesquery, variables, flatten = TRUE))$data
  output <- result$indexVariantsAndStudiesForTagVariant$associations %>% dplyr::tibble()


  if (nrow(output) == 0) {
    output <- data.frame()
  }

  return(output)
}
