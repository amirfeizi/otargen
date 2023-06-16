#' Get GWAS credible set data for a variant in a study
#'
#' Provided with a study ID and a lead variant ID, this function returns a
#' data frame consisting of all the associated credible set tag variants
#'  with the corresponding statistical data.
#'
#' @param \emph{studyid} String: Open Target Genetics generated ID for the GWAS study.
#' @param \emph{variantid} String: Open Target Genetics generated ID for the variant (CHRPOSITION_REFALLELE_ALTALLELE or rsId).
#'
#' @return Returns a data frame of results from the credible set of variants for a specific lead variant with the following columns:
#' \itemize{
#'   \item{\code{tagVariant.id}:} \emph{Data frame}. A table of IDs of the tag variant.
#'   \item{\code{tagVariant.rsId}:} \emph{Character vector}. rsID of the tag variant.
#'   \item{\code{beta}:} \emph{Numeric}. Beta value.
#'   \item{\code{postProb}:} \emph{Numeric}. Posterior probability.
#'   \item{\code{pval}:} \emph{Numeric}. P-value.
#'   \item{\code{se}:} \emph{Numeric}. Standard error.
#'   \item{\code{MultisignalMethod}:} \emph{Character vector}. Multisignal method.
#'   \item{\code{logABF}:} \emph{Numeric}. Logarithm of approximate Bayes factor.
#'   \item{\code{is95}:} \emph{Logical}. Indicates if the variant has a 95% credible set.
#'   \item{\code{is99}:} \emph{Logical}. Indicates if the variant has a 99% credible set.
#' }
#' @examples
#' \dontrun{
#' result <- gwasCredibleSet(studyid="GCST90002357", variantid="1_154119580_C_A")
#' result <- gwasCredibleSet(studyid="GCST90002357", variantid="rs2494663")
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
