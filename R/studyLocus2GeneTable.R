#' Retrieves L2G data table.
#'
#' For an input variant id and associated study id, generated a table
#' with the following columns - studyId, variant.id, variant.rsId, yProbaDistance,
#' yProbaModel, yProbaMolecularQTL, yProbaPathogenicity, yProbaInteraction, hasColoc
#' distanceToLocus, gene.id, and gene.symbol.
#'
#'
#' @param studyid String: Open Target Genetics generated id for GWAS study.
#' @param variantid String: Open Target Genetics generated id for variant (CHR_POSITION_REFALLELE_ALT_ALLELE or rsId).
#'
#' @return Dataframe with summary stats of the study and datatable of the various calculated scores and features for any lead variant.
#'
#' @examples
#' studyLocus2GeneTable(studyid = "GCST90002357", variantid = "1_154119580_C_A")
#' or
#' studyLocus2GeneTable(studyid = "GCST90002357", variantid = "rs2494663")
#'
#' @export
#'
#'
#'
studyLocus2GeneTable <- function(studyid, variantid) {
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

  ## Query for GWAS study locus details
  query <- "query l2gQuery($studyId: String!, $variantId: String!){
  studyLocus2GeneTable(studyId: $studyId, variantId: $variantId){
    study{
    studyId
    traitReported
  }
    variant {
      id
      rsId
    }
    rows {
      gene {
        id
        symbol
      }
    yProbaDistance
    yProbaModel
    yProbaMolecularQTL
    yProbaPathogenicity
    yProbaInteraction
    hasColoc
    distanceToLocus
}
  }
}"


  ## Execute the query

  variables <- list(studyId = studyid, variantId = input_variantid)

  otg_qry$query(name = "l2g_query", x = query)

  cli::cli_progress_step(paste("Downloading data for ", studyid, ",", variantid, "..."), spinner = TRUE)

  study_l2g <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$l2g_query, variables), flatten = TRUE)$data
  df_l2g <- data.frame()
  df_rows <- as.data.frame(study_l2g$studyLocus2GeneTable$rows)
  if (nrow(df_rows) != 0) {
    if (is.null(study_l2g$studyLocus2GeneTable$variant$rsId)) {
      study_l2g$studyLocus2GeneTable$variant$rsId <- NA
    }
    df_l2g <- as.data.frame(study_l2g$studyLocus2GeneTable)
    df_l2g <- df_l2g %>% dplyr::mutate(across(where(is.numeric), ~ round(., 2)))
    base::colnames(df_l2g) <- stringr::str_replace_all(colnames(df_l2g), "rows.", "")
  }

  return(df_l2g)
}
