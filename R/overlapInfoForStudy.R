#' Retrieves overlap info for a study and a list of studies
#'
#' For an input study id and a list of other study ids, this function returns two elements.
#' One contains the overlap information in a table format with the following columns-
#' studyId, traitReported, traitCategory, variantIdA, variantIdB, overlapAB, distinctA, distinctB, study.studyId,
#' study.traitReported, study.traitCategory. The other element is the variant intersection set.
#' It represents an overlap between two variants of the two given studies.
#'
#' @param studyid String: Open Target Genetics generated id for GWAS study.
#' @param studyids List of Strings: list of Open Target Genetics generated id for GWAS studies.
#'
#' @return A list containing a data frame of overlap information and the variant intersection set.
#'
#' @examples
#' \dontrun{
#' otargen::overlapInfoForStudy(studyid="GCST90002357", studyids=list("GCST90025975","GCST90025962"))
#' }
#' @export
#'
#'
#'
overlapInfoForStudy <- function(studyid, studyids=list()) {


  ## Set up to query Open Targets Genetics API

  cli::cli_progress_step("Connecting the database...", spinner = TRUE)
  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()

  variables <- list(studyId = studyid, studyIds = studyids)
  variables <- list(studyId="GCST90002357", studyIds=list("GCST90025975","GCST90025962"))

  query <- "query overlapinfostudyquery($studyId: String!, $studyIds: [String!]!){
  overlapInfoForStudy(studyId: $studyId, studyIds: $studyIds) {
  study{
  studyId
  traitReported
  traitCategory
  }
  overlappedVariantsForStudies{
    overlaps{
      variantIdA
      variantIdB
      overlapAB
      distinctA
      distinctB
    }
    study{
      studyId
      traitReported
      traitCategory
    }
  }
  variantIntersectionSet
  }
}"

  final_output <- list()

  otg_qry$query(name = "overlapinfostudy_query", x = query )

  ## Execute the query

  cli::cli_progress_step("Downloading data...", spinner = TRUE)

  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$overlapinfostudy_query, variables), flatten=TRUE)$data

  df_study <- data.frame(result$overlapInfoForStudy$study)
  var_int_set <- result$overlapInfoForStudy$variantIntersectionSet
  result <- dplyr::tibble(place = result)

  df_overlap <- result %>% tidyr::unnest_wider(place) %>%
    dplyr::select(overlappedVariantsForStudies) %>%
    tidyr::unnest(overlappedVariantsForStudies, keep_empty=TRUE) %>%
    tidyr::hoist(overlaps, variantIdA = 'variantIdA', variantIdB = 'variantIdB',
                 overlapAB = 'overlapAB', distinctA = 'distinctA', distinctB = 'distinctB') %>%
    tidyr::unnest(c(variantIdA, variantIdB, overlapAB, distinctA, distinctB),
                  keep_empty=TRUE) %>% dplyr::as_tibble()

  row_num <- nrow(df_overlap)

  df_study <- df_study[rep(seq_len(nrow(df_study)), each = row_num),]
  new_df <- cbind(df_study, df_overlap)
  rownames(new_df) <- NULL

  final_output <- list(overlap_info = new_df, variant_intersection_set = var_int_set)



  return(final_output)
}
