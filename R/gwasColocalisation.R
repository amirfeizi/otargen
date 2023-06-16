#' Retrieves GWAS colocalisation data.
#'
#' This function retrieves colocalisation data for a given GWAS study and variant.
#'  It performs colocalisation analysis using the coloc method (Giambartolomei et al., 2014).
#'   Coloc is a Bayesian method which, for two traits, integrates evidence over all variants
#'   at a locus to evaluate the following hypotheses:
#'   - H0: No association with either trait
#'   - H1: Association with trait 1, not with trait 2
#'   - H2: Association with trait 2, not with trait 1
#'   - H3: Association with trait 1 and trait 2, two independent SNPs
#'   - H4: Association with trait 1 and trait 2, one shared SNP
#'
#' Under this framework, evidence for H4 is considered evidence for colocalisation between the two traits.
#'
#'
#' @param studyid String: Open Target Genetics generated ID for the GWAS study.
#' @param variantid String: Open Target Genetics generated ID for the variant (CHRPOSITION_REFALLELE_ALTALLELE or rsID).
#'
#' @return Returns a data frame of the studies that colocalise with the input variant and study. The table consists of the following data structure:
#' \itemize{
#'   \item{\code{study.studyId}:} \emph{Character vector}. Study identifier.
#'   \item{\code{study.traitReported}:} \emph{Character vector}. Reported trait associated with the colocalisation.
#'   \item{\code{study.traitCategory}:} \emph{Character vector}. Trait category.
#'   \item{\code{indexVariant.id}:} \emph{Character vector}. Index variant identifier.
#'   \item{\code{indexVariant.position}:} \emph{Integer vector}. Index variant position.
#'   \item{\code{indexVariant.chromosome}:} \emph{Character vector}. Index variant chromosome.
#'   \item{\code{indexVariant.rsId}:} \emph{Character vector}. Index variant rsID.
#'   \item{\code{beta}:} \emph{Numeric vector}. Beta value.
#'   \item{\code{h3}:} \emph{Numeric vector}. H3 value associated with the colocalisation.
#'   \item{\code{h4}:} \emph{Numeric vector}. H4 value associated with the colocalisation.
#'   \item{\code{log2h4h3}:} \emph{Numeric vector}. Log2 ratio of H4 to H3 values.
#' }
#'
#' @examples
#' \dontrun{
#' colocalisation_data <- gwasColocalisation(studyid = "GCST90002357", variantid = "1_154119580_C_A")
#' colocalisation_data <- gwasColocalisation(studyid = "GCST90002357", variantid = "rs2494663")
#' }
#' @import dplyr
#' @importFrom magrittr %>%
#' @export
#' @references
#' Giambartolomei, Claudia et al. “Bayesian test for colocalisation between pairs of genetic association studies using summary statistics.” PLoS genetics vol. 10,5 e1004383. 15 May. 2014, doi:10.1371/journal.pgen.1004383


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
