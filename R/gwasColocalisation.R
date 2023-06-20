#' Retrieve calculated GWAS colocalisation data
#'
#' This function retrieves information about colocalisation between two independent associations from GWAS studies.
#'  It returns a data frame of the studies that colocalise with the input variant and study,
#'   including details on the study and reported trait, index variant, and calculated coloc method (see Ref. below) outputs.
#'
#' @param study_id Character: Open Target Genetics generated ID for the GWAS study.
#' @param variant_id Character: Open Target Genetics generated ID for the variant (CHRPOSITION_REFALLELE_ALTALLELE or rsID).
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
#'   \item{\code{beta}:} \emph{Numeric vector}. Beta value associated with the colocalisation.
#'   \item{\code{h3}:} \emph{Numeric vector}. H3 value associated with the colocalisation.
#'   \item{\code{h4}:} \emph{Numeric vector}. H4 value associated with the colocalisation.
#'   \item{\code{log2h4h3}:} \emph{Numeric vector}. Log2 ratio of H4 to H3 values.
#' }
#'
#' @examples
#' \dontrun{
#' colocalisation_data <- gwasColocalisation(study_id = "GCST90002357", variant_id = "1_154119580_C_A")
#' colocalisation_data <- gwasColocalisation(study_id = "GCST90002357", variant_id = "rs2494663")
#' }
#' @import dplyr
#' @importFrom magrittr %>%
#' @export
#' @references
#' Giambartolomei, Claudia et al. “Bayesian test for colocalisation between pairs of genetic association studies using summary statistics.” PLoS genetics vol. 10,5 e1004383. 15 May. 2014, doi:10.1371/journal.pgen.1004383
#'
#'

gwasColocalisation <- function(study_id, variant_id) {
  ## Set up to query Open Targets Genetics API

  cli::cli_progress_step("Connecting the database...", spinner = TRUE)
  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()


  # Check variant id format
  if (grepl(pattern = "rs\\d+", variant_id)) {
    # Convert rs id to variant id
    query_searchid <- "query ConvertRSIDtoVID($queryString: String!) {
      search(queryString: $queryString) {
        totalVariants
        variants {
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
    stop("\nPlease provide a variant ID.")
  }


  query <- "query gwascolquery($studyId: String!, $variantId: String!) {
    gwasColocalisation(studyId: $studyId, variantId: $variantId) {
      indexVariant {
        id
        position
        chromosome
        rsId
      }
      study {
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
  output <- data.frame()
  variables <- list(studyId = study_id, variantId = input_variant_id)

  otg_qry$query(name = "gwascol_query", query)
  cli::cli_progress_step("Downloading data...", spinner = TRUE)
  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$gwascol_query, variables), flatten = TRUE)$data

  output <- result$gwasColocalisation %>% dplyr::tibble()

  if (nrow(output) != 0) {
    output <- output[, c("study.studyId", "study.traitReported", "study.traitCategory",
                         "indexVariant.id", "indexVariant.position",
                         "indexVariant.chromosome", "indexVariant.rsId",
                         "beta", "h3", "h4", "log2h4h3")]
  }

  return(output)
}

