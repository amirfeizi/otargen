#' Retrieves QTL credible set data
#'
#' For an input gene id and it's associated study id, variant id and biofeature, a data table with following columns
#' are generated- tagVariant.id, tagVariant.rsId, pval, se (standard error), beta, postProb, MultisignalMethod, logABF, is95, is99.
#'
#' @param studyid String: Open Target Genetics generated id for GWAS study.
#' @param variantid String: Open Target Genetics generated id for variant (CHR_POSITION_REFALLELE_ALT_ALLELE or rsId).
#' @param geneid String: a gene ENSEMBL id.
#' @param biofeature String: represents either a tissue, cell type, aggregation type, protein type etc.
#'
#' @return Data frame of results from qtl credible set of variants for a given study, variant, gene and biofeature.
#'
#' @examples
#' qtlCredibleSet(studyid="Braineac2", variantid="1_55053079_C_T", geneid="ENSG00000169174", biofeature="SUBSTANTIA_NIGRA")
#'
#'
#' @export
#'
#'
qtlCredibleSet <- function(studyid, variantid, geneid, biofeature) {
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


  query <- "query qtlcredsetquery($studyId: String!, $variantId: String!, $geneId: String!, $bioFeature: String!){
  qtlCredibleSet(studyId: $studyId, variantId: $variantId, geneId: $geneId, bioFeature: $bioFeature) {
  tagVariant {
      id
      rsId
    }
  pval
  se
  beta
  postProb
  MultisignalMethod
  logABF
  is95
  is99
}
}"

  variables <- list(studyId = studyid, variantId = input_variantid, geneId = geneid, bioFeature = biofeature)
  otg_qry$query(name = "qtlcredset_query", x = query)

  ## Execute the query

  cli::cli_progress_step("Downloading data...", spinner = TRUE)
  qtl_cred_set <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$qtlcredset_query, variables, flatten = TRUE))$data
  df_qtl_cred <- as.data.frame(qtl_cred_set)
  base::colnames(df_qtl_cred) <- stringr::str_replace_all(colnames(df_qtl_cred),"qtlCredibleSet.","")

  return(df_qtl_cred)
}
