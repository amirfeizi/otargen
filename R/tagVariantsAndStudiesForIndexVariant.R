#' Retrieves tag variants and studies for a given index variant.
#'
#' For an input index variant ID, this function fetches information about the tag variants
#' and associated studies, including scores.
#'
#' @param variant_id Character: Open Targets Genetics generated ID for a variant (CHRPOSITION_REFALLELE_ALTALLELE or rsId).
#' @param pageindex Integer: Index of the current page for pagination (>= 0).
#' @param pagesize Integer: Number of records in a page for pagination (> 0).
#'
#' @return Returns a data frame containing the variant associations connected to the input index variant.
#' The columns in the data frame are as follows:
#' \itemize{
#'   \item{\code{tagVariant.id}:} \emph{Character}. Tag variant ID.
#'   \item{\code{tagVariant.chromosome}:} \emph{Character}. Chromosome of the tag variant.
#'   \item{\code{tagVariant.rsId}:} \emph{Character}. rsID of the tag variant.
#'   \item{\code{tagVariant.position}:} \emph{Integer}. Position of the tag variant.
#'   \item{\code{study.studyId}:} \emph{Character}. Study ID.
#'   \item{\code{study.traitReported}:} \emph{Character}. Reported trait of the study.
#'   \item{\code{study.traitCategory}:} \emph{Character}. Category of the trait in the study.
#'   \item{\code{pval}:} \emph{Numeric}. P-value.
#'   \item{\code{pvalMantissa}:} \emph{Numeric}. Mantissa of the p-value.
#'   \item{\code{pvalExponent}:} \emph{Integer}. Exponent of the p-value.
#'   \item{\code{nTotal}:} \emph{Integer}. Total number of samples.
#'   \item{\code{nCases}:} \emph{Integer}. Number of cases in the study.
#'   \item{\code{overallR2}:} \emph{Numeric}. Overall R-squared value.
#'   \item{\code{afr1000GProp}:} \emph{Numeric}. Proportion in African 1000 Genomes population.
#'   \item{\code{amr1000GProp}:} \emph{Numeric}. Proportion in Admixed American 1000 Genomes population.
#'   \item{\code{eas1000GProp}:} \emph{Numeric}. Proportion in East Asian 1000 Genomes population.
#'   \item{\code{eur1000GProp}:} \emph{Numeric}. Proportion in European 1000 Genomes population.
#'   \item{\code{sas1000GProp}:} \emph{Numeric}. Proportion in South Asian 1000 Genomes population.
#'   \item{\code{oddsRatio}:} \emph{Numeric}. Odds ratio.
#'   \item{\code{oddsRatioCILower}:} \emph{Numeric}. Lower bound of the odds ratio confidence interval.
#'   \item{\code{oddsRatioCIUpper}:} \emph{Numeric}. Upper bound of the odds ratio confidence interval.
#'   \item{\code{posteriorProbability}:} \emph{Numeric}. Posterior probability.
#'   \item{\code{beta}:} \emph{Numeric}. Beta value.
#'   \item{\code{betaCILower}:} \emph{Numeric}. Lower bound of the beta value confidence interval.
#'   \item{\code{betaCIUpper}:} \emph{Numeric}. Upper bound of the beta value confidence interval.
#'   \item{\code{direction}:} \emph{Character}. Direction of the effect.
#'   \item{\code{log10Abf}:} \emph{Numeric}. Log base 10 of the approximate Bayes factor.
#' }
#'
#' @examples
#' \dontrun{
#' result <- tagVariantsAndStudiesForIndexVariant(variant_id = "1_109274968_G_T")
#' result <- tagVariantsAndStudiesForIndexVariant(variant_id = "1_109274968_G_T"
#'     ,pageindex = 1, pagesize = 50)
#'     }
#' @importFrom magrittr %>%
#' @export
#'

tagVariantsAndStudiesForIndexVariant <- function(variant_id, pageindex = 0, pagesize = 20) {

  # Check if the variant ID argument is empty or null
  if (missing(variant_id) || is.null(variant_id) || variant_id == "") {
    message("Please provide a value for the variant ID argument.")
    return(NULL)
  }

  ## Set up to query Open Targets Genetics API
  cli::cli_progress_step("Connecting the database...", spinner = TRUE)
  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()

  # Check variant id format
  if (grepl(pattern = "rs\\d+", variant_id)) {
    # Convert rs id to variant id
    query_searchid <- "query ConvertRSIDtoVID($queryString:String!) {
    search(queryString:$queryString){
      totalVariants
      variants{
        id
      }
    }
  }"

    variables <- list(queryString = variant_id)
    otg_qry$query(name = "convertid", x = query_searchid)
    id_result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$convertid, variables), flatten = TRUE)$data
    input_variant_id <- id_result$search$variants$id
  } else if (grepl(pattern = "\\d+_\\d+_[a-zA-Z]+_[a-zA-Z]+", variant_id)) {
    input_variant_id <- variant_id
  } else {
    stop("\n Please provide a variant Id")
  }

  query <- "query tagVariantsAndStudiesForIndexVariantquery($variantId: String!, $pageIndex: Int!, $pageSize:Int!){
    tagVariantsAndStudiesForIndexVariant(variantId: $variantId, pageIndex: $pageIndex, pageSize: $pageSize) {
      associations {
        tagVariant {
          id
          chromosome
          rsId
          position
        }
        study {
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
  variables <- list(variantId = input_variant_id, pageIndex = pageindex, pageSize = pagesize)

  otg_qry$query(name = "tagVariantsAndStudiesForIndexVariant_query", x = query)

  cli::cli_progress_step("Downloading data...", spinner = TRUE)
  tag_var_studies <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$tagVariantsAndStudiesForIndexVariant_query, variables, flatten = TRUE))$data

  tag_var_studies <- tag_var_studies$tagVariantsAndStudiesForIndexVariant$associations %>% as.data.frame()

  return(tag_var_studies)
}
