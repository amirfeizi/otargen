#' Retrieves overlap info for a study and a list of studies
#'
#' For an input study ID and a list of other study IDs, this function returns two elements.
#' One contains the overlap information in a table format, and the other element is the variant intersection set,
#' representing an overlap between two variants of the two given studies.
#'
#' @param study_id Character: Study ID(s) generated by Open Targets (e.g GCST90002357).
#' @param study_ids Character: generated ID for variants by Open Targets (e.g. 1_154119580_C_A) or rsId (rs2494663).
#'
#' @return A list containing a data frame of overlap information and the variant intersection set.
#' The overlap information table (overlap_info) consists of the following columns:
#' \itemize{
#'   \item{\code{studyId}:} \emph{Character vector}. Study ID.
#'   \item{\code{traitReported}:} \emph{Character vector}. Reported trait.
#'   \item{\code{traitCategory}:} \emph{Character vector}. Trait category.
#'   \item{\code{variantIdA}:} \emph{Character vector}. Variant ID from study A.
#'   \item{\code{variantIdB}:} \emph{Character vector}. Variant ID from study B.
#'   \item{\code{overlapAB}:} \emph{Integer vector}. Number of overlaps between variants A and B.
#'   \item{\code{distinctA}:} \emph{Integer vector}. Number of distinct variants in study A.
#'   \item{\code{distinctB}:} \emph{Integer vector}. Number of distinct variants in study B.
#'   \item{\code{study.studyId}:} \emph{Character vector}. Study ID from study list.
#'   \item{\code{study.traitReported}:} \emph{Character vector}. Reported trait from study list.
#'   \item{\code{study.traitCategory}:} \emph{Character vector}. Trait category from study list.
#' }
#' The variant intersection set (variant_intersection_set) is a character vector representing the intersection of variants.
#'
#' @examples
#' \dontrun{
#' result <- overlapInfoForStudy(study_id = "GCST90002357",
#'  study_ids = list("GCST90025975", "GCST90025962"))
#' }
#' @import dplyr
#' @importFrom magrittr %>%
#' @export
#'
#'
#'

overlapInfoForStudy <- function(study_id, study_ids = list()) {
  ## Set up to query Open Targets API
tryCatch({
  cli::cli_progress_step("Connecting to the Open Targets GrpahQL API...", spinner = TRUE)
  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()

  variables <- list(studyId = study_id, studyIds = study_ids)

  query <- "query overlapinfostudyquery($studyId: String!, $studyIds: [String!]!) {
    overlapInfoForStudy(studyId: $studyId, studyIds: $studyIds) {
      study {
        studyId
        traitReported
        traitCategory
      }
      overlappedVariantsForStudies {
        overlaps {
          variantIdA
          variantIdB
          overlapAB
          distinctA
          distinctB
        }
        study {
          studyId
          traitReported
          traitCategory
        }
      }
      variantIntersectionSet
    }
  }"

  final_output <- list()

  otg_qry$query(name = "overlapinfostudy_query", x = query)

  ## Execute the query
  cli::cli_progress_step("Downloading data...", spinner = TRUE)
  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$overlapinfostudy_query, variables), flatten = TRUE)$data

  df_study <- data.frame(result$overlapInfoForStudy$study)
  var_int_set <- result$overlapInfoForStudy$variantIntersectionSet
  result <- dplyr::tibble(place = result)

  df_overlap <- result %>%
    tidyr::unnest_wider(place) %>%
    dplyr::select(overlappedVariantsForStudies) %>%
    tidyr::unnest(overlappedVariantsForStudies, keep_empty = TRUE) %>%
    tidyr::hoist(overlaps, variantIdA = 'variantIdA', variantIdB = 'variantIdB',
                 overlapAB = 'overlapAB', distinctA = 'distinctA', distinctB = 'distinctB') %>%
    tidyr::unnest(c(variantIdA, variantIdB, overlapAB, distinctA, distinctB), keep_empty = TRUE) %>%
    dplyr::as_tibble()

  row_num <- nrow(df_overlap)

  df_study <- df_study[rep(seq_len(nrow(df_study)), each = row_num), ]
  new_df <- cbind(df_study, df_overlap)
  rownames(new_df) <- NULL

  final_output <- list(overlap_info = new_df, variant_intersection_set = var_int_set)

  return(final_output)

}, error = function(e) {
  # Handling connection timeout
  if(grepl("Timeout was reached", e$message)) {
    stop("Connection timeout reached while connecting to the Open Targets GraphQL API.")
  } else {
    stop(e) # Handle other types of errors
  }
})
}
