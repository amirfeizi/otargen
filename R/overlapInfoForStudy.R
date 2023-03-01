#' Retrieves overlap info for a study and a list of studies
#'
#' For an input study id and a list of other study ids, this function returns a data table as shown in the example.
#' It represents an overlap between two variants of the two given studies.
#'
#' @param studyid String: Open Target Genetics generated id for GWAS study.
#' @param studyids List of Strings: list of Open Target Genetics generated id for GWAS studies.
#'
#' @return A list containing a data frame of overlap information and the variant intersection set.
#'
#' @examples
#' overlap_output <- overlapInfoForStudy(studyid="GCST90002357", studyids=list("GCST90025975","GCST90025962"))
#' overlap_output
#' $overlap_info
#'       studyId  traitReported traitCategory       variantIdA       variantIdB overlapAB distinctA distinctB study.studyId                       study.traitReported study.traitCategory
#'  GCST90002357 Platelet count   measurement   2_27508073_T_C   2_27508073_T_C         1         0         0  GCST90025975                  Mean reticulocyte volume         measurement
#'  GCST90002357 Platelet count   measurement  15_75077277_G_A  15_75039464_T_G         2        51         1  GCST90025975                  Mean reticulocyte volume         measurement
#'  GCST90002357 Platelet count   measurement  15_78243095_C_G  15_78244187_C_T         1        12         0  GCST90025975                  Mean reticulocyte volume         measurement
#' ...
#'
#' $variant_intersection
#' "17_44108762_C_A"  "9_133256028_C_T"  "10_103916707_C_A" "12_11613555_A_C"  "3_179024902_T_C"  "2_240571486_G_A"  "1_247876149_C_T"  "12_4225142_G_C"
#' "17_46144477_A_G"  "13_32571858_G_A"  "8_30422400_T_C"   "14_105290148_A_G" "4_54542832_T_C"   "10_69334748_T_C"  "16_68704303_G_A"  "18_63253621_C_T"
#' ...
#'
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

  final_output <- list(overlap_info = new_df, variant_intersection = var_int_set)



  return(final_output)
}
