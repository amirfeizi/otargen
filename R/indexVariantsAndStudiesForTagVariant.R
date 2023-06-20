#' Retrieve population-level summary GWAS statistics.
#'
#' For an input tag variant ID, this function returns a data frame(tibble format) with population-level summary statistics data across various GWAS studies.
#'
#' @param variant_id Character: generated ID for variants by Open Targets Genetics (e.g. 1_154119580_C_A) or rsId (rs2494663).
#' @param pageindex Integer: Index of the current page, pagination index >= 0.
#' @param pagesize Integer: Number of records in a page, pagination size > 0.
#'
#' @return Returns a data frame containing the variant associated with the input tag variant. The table consists of the following columns:
#' \itemize{
#'   \item{\code{index_variant}:} \emph{Data frame}. Data frame of index variants with the following columns:
#'     \itemize{
#'       \item{\code{id}:} \emph{Character vector}. Variant ID.
#'       \item{\code{rsId}:} \emph{Character vector}. rsID of the variant.
#'     }
#'   \item{\code{study}:} \emph{Data frame}. Data frame of studies with the following columns:
#'     \itemize{
#'       \item{\code{studyId}:} \emph{Character vector}. Study identifier.
#'       \item{\code{traitReported}:} \emph{Character vector}. Reported trait associated with the colocalisation.
#'       \item{\code{traitCategory}:} \emph{Character vector}. Trait category.
#'     }
#'   \item{\code{pval}:} \emph{Numeric vector}. P-value.
#'   \item{\code{pval_mantissa}:} \emph{Numeric vector}. Mantissa of the p-value.
#'   \item{\code{pval_exponent}:} \emph{Integer vector}. Exponent of the p-value.
#'   \item{\code{n_total}:} \emph{Integer vector}. Total number of samples.
#'   \item{\code{n_cases}:} \emph{Integer vector}. Number of cases.
#'   \item{\code{overall_r2}:} \emph{Numeric vector}. Overall R-squared value.
#'   \item{\code{afr1000g_prop}:} \emph{Numeric vector}. Proportion in African population in 1000 Genomes.
#'   \item{\code{amr1000g_prop}:} \emph{Numeric vector}. Proportion in Admixed American population in 1000 Genomes.
#'   \item{\code{eas1000g_prop}:} \emph{Numeric vector}. Proportion in East Asian population in 1000 Genomes.
#'   \item{\code{eur1000g_prop}:} \emph{Numeric vector}. Proportion in European population in 1000 Genomes.
#'   \item{\code{sas1000g_prop}:} \emph{Numeric vector}. Proportion in South Asian population in 1000 Genomes.
#'   \item{\code{log10abf}:} \emph{Numeric vector}. Log10 ABF (Approximate Bayes Factor).
#'   \item{\code{posterior_probability}:} \emph{Numeric vector}. Posterior probability.
#'   \item{\code{odds_ratio}:} \emph{Numeric vector}. Odds ratio.
#'   \item{\code{odds_ratio_ci_lower}:} \emph{Numeric vector}. Lower confidence interval of the odds ratio.
#'   \item{\code{odds_ratio_ci_upper}:} \emph{Numeric vector}. Upper confidence interval of the odds ratio.
#'   \item{\code{beta}:} \emph{Numeric vector}. Beta value.
#'   \item{\code{beta_ci_lower}:} \emph{Numeric vector}. Lower confidence interval of the beta value.
#'   \item{\code{beta_ci_upper}:} \emph{Numeric vector}. Upper confidence interval of the beta value.
#'   \item{\code{direction}:} \emph{Character vector}. Direction of the effect.
#' }
#'
#' @examples
#' \dontrun{
#' result <- indexVariantsAndStudiesForTagVariant(variant_id = "1_109274968_G_T")
#' result <- indexVariantsAndStudiesForTagVariant(variant_id = "rs12740374",
#'  pageindex = 1, pagesize = 50)
#' }
#' @import dplyr
#' @importFrom magrittr %>%
#' @export
#'

indexVariantsAndStudiesForTagVariant <- function(variant_id, pageindex = 0, pagesize = 20) {

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
      search(queryString:$queryString) {
        totalVariants
        variants {
          id
        }
      }
    }"

    variables <- list(queryString = variant_id)
    otg_qry$query(name = "convertid", x = query_searchid)
    id_result <- jsonlite::fromJSON(
      otg_cli$exec(otg_qry$queries$convertid, variables),
      flatten = TRUE
    )$data
    input_variant_id <- id_result$search$variants$id
  } else if (grepl(pattern = "\\d+_\\d+_[a-zA-Z]+_[a-zA-Z]+", variant_id)) {
    input_variant_id <- variant_id
  } else {
    stop("\nPlease provide a variant ID")
  }

  query <- "query indexvariantsandstudiesquery($variantId: String!, $pageIndex: Int!, $pageSize: Int!) {
    indexVariantsAndStudiesForTagVariant(variantId: $variantId, pageIndex: $pageIndex, pageSize: $pageSize) {
      associations {
        indexVariant {
          id
          rsId
        }
        study {
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
  variables <- list(variantId = input_variant_id, pageIndex = pageindex, pageSize = pagesize)

  otg_qry$query(name = "indexvariantsandstudiesquery", x = query)

  cli::cli_progress_step("Downloading data...", spinner = TRUE)
  result <- jsonlite::fromJSON(
    otg_cli$exec(otg_qry$queries$indexvariantsandstudiesquery, variables, flatten = TRUE),
    simplifyDataFrame = TRUE
  )$data

  output <- result$indexVariantsAndStudiesForTagVariant$associations %>%
    dplyr::tibble() %>%
    janitor::clean_names()

  if (nrow(output) == 0) {
    output <- data.frame()
  }

  return(output)
}







