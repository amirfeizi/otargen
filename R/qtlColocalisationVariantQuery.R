#' Retrieves colocalisation data for a variant in a study
#'
#' For an input study id and variant id, a table is generated with the following columns-
#' qtlStudyName, phenotypeId, gene.id, gene.symbol, name, indexVariant.id, indexVariant.rsId,
#' beta, h4, h3, log2h4h3.
#'
#' @param studyid String: Open Target Genetics generated id for gwas studies.
#' @param variantid String: Open Target Genetics generated id for variant (CHR_POSITION_REFALLELE_ALT_ALLELE or rsId).
#'
#' @return A data frame of the colocalisation information for a lead variant.
#'
#' @examples
#' \dontrun{
#' otargen::qtlColocalisationVariantQuery(studyid="GCST90002357",variantid="1_154119580_C_A")
#' otargen::qtlColocalisationVariantQuery(studyid="GCST90002357",variantid="rs2494663")
#'}
#' @export
#'
#'

qtlColocalisationVariantQuery <- function(studyid, variantid) {

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


  ## Query for QTL colocalisation

  query <- "query qtlColocalisationVariantQuery($studyId: String!, $variantId: String!) {
  qtlColocalisation(studyId: $studyId, variantId: $variantId){
    qtlStudyName
    phenotypeId
    gene {
      id
      symbol
    }
    tissue {
      name
    }
    indexVariant {
      id
      rsId
    }
    beta
    h4
    h3
    log2h4h3
  }
}"

  # execute the query

  variables <- list(studyId = studyid, variantId = input_variantid)


  otg_qry$query(name = "qtl_query", x = query)

  cli::cli_progress_step("Downloading data...", spinner = TRUE)
  qtlcoloc_result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$qtl_query, variables, flatten = TRUE))$data
  df_qtlcoloc <- as.data.frame(qtlcoloc_result$qtlColocalisation)
  return(df_qtlcoloc)
}
