#' Retrieves tag variants and studies for a given index variant.
#'
#' For an input index variant id, fetches the tag variant information along with study information and scores.
#'
#'
#' @param variantid String: Open Target Genetics generated id for variant (CHR_POSITION_REFALLELE_ALT_ALLELE or rsId).
#' @param pageindex Int: Index of the current page, pagination index >= 0.
#' @param pagesize Int: No. of records in a page, pagination size > 0.
#'
#' @return Returns a data Frame containing the variant associations connected to the input index variant with the following columns:
#'
#' \enumerate{
#' \item tagVariant.id
#' \item tagVariant.chromosome
#' \item tagVariant.rsId
#' \item tagVariant.position
#' \item study.studyId
#' \item study.traitReported
#' \item study.traitCategory
#' \item pval
#' \item pvalMantissa
#' \item pvalExponent
#' \item nTotal
#' \item nCases
#' \item overallR2
#' \item afr1000GProp
#' \item amr1000GProp
#' \item eas1000GProp
#' \item eur1000GProp
#' \item sas1000GProp
#' \item oddsRatio
#' \item oddsRatioCILower
#' \item oddsRatioCIUpper
#' \item posteriorProbability
#' \item beta
#' \item betaCILower
#' \item betaCIUpper
#' \item direction
#' \item log10Abf
#' }
#'
#' @examples
#' \dontrun{
#' otargen::tagVariantsAndStudiesForIndexVariant(variantid = "1_109274968_G_T")
#' otargen::tagVariantsAndStudiesForIndexVariant(variantid = "1_109274968_G_T",pageindex = 1, pagesize = 50)
#'}
#' @importFrom magrittr %>%
#' @export
#'
#'

tagVariantsAndStudiesForIndexVariant <- function(variantid, pageindex = 0, pagesize = 20) {
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


  query <- "query tagVariantsAndStudiesForIndexVariantquery($variantId: String!, $pageIndex: Int!, $pageSize:Int!){
  tagVariantsAndStudiesForIndexVariant(variantId: $variantId, pageIndex: $pageIndex, pageSize: $pageSize) {
    associations{
  tagVariant{
        id
    chromosome
    rsId
    position
      }
      study{
        studyId
        traitReported
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
      oddsRatio
    oddsRatioCILower
    oddsRatioCIUpper
      posteriorProbability
      beta
    betaCILower
    betaCIUpper
      direction
      log10Abf
  }
  }
}"

  ## Execute the query
  variables <- list(variantId = input_variantid, pageIndex = pageindex, pageSize = pagesize)

  otg_qry$query(name = "tagVariantsAndStudiesForIndexVariant_query", x = query)

  cli::cli_progress_step("Downloading data...", spinner = TRUE)
  tag_var_studies <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$tagVariantsAndStudiesForIndexVariant_query, variables, flatten = TRUE))$data

  tag_var_studies <- tag_var_studies$tagVariantsAndStudiesForIndexVariant$associations %>% as.data.frame()

  return(tag_var_studies)
}
