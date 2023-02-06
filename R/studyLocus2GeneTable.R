#' Get locus data table of a variant around a gene
#'
#' @param studyid is the Open Target Genetics generated id for gwas studies.
#' @param variantid is Open Target Genetics generated id for each variant in the database.
#' @return A dataframe with summary stats of the study and datatable of the various calculated scores and features for any lead variant.
#' @examples
#' studyLocus2GeneTable("GCST90002357", "1_154119580_C_A")
#' @export
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

  ## Query for GWAS study locus details
  query <- "query l2gQuery($studyId: String!, $variantId: String!){
  studyLocus2GeneTable(studyId: $studyId, variantId: $variantId){
    study{
    studyId
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

  cli::cli_progress_step(paste("Downloading data for ",studyid, variantid,"..."), spinner = TRUE)

  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$l2g_query, variables), flatten=TRUE)$data
  result_df <- data.frame()
  df_rows = as.data.frame(result$studyLocus2GeneTable$rows)
  if (nrow(df_rows) != 0){
    if (is.null(result$studyLocus2GeneTable$variant$rsId)){
      result$studyLocus2GeneTable$variant$rsId = NA
    }
    result_df <- as.data.frame(result$studyLocus2GeneTable)
  }
  return (result_df)
}
