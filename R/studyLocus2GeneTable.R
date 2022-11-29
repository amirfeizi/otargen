#' Get locus data table of a variant around a gene
#'
#' @param studyid is the Open Target Genetics generated id for gwas studies.
#' @param variantid is Open Target Genetics generated id for each variant in the databse.
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
  variables <- list(studyId = studyid, variantId = variantid)

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

  otg_qry$query(name = "l2g_query", x = query)

  cli::cli_progress_step("Downloading data...", spinner = TRUE)
  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$l2g_query, variables))$data
  result_df <- as.data.frame(result$studyLocus2GeneTable)
  return (result_df)
}
