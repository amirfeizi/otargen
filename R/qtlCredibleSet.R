#' Retrieves QTL credible set data
#'
#' For an input gene id and it's associated study id, variant id and biofeature, a table is generated
#' as shown in the example.
#'
#' @param studyid String: Open Target Genetics generated id for GWAS study.
#' @param variantid String: Open Target Genetics generated id for variant (CHR_POSITION_REFALLELE_ALT_ALLELE or rsId).
#' @param geneid String: a gene ENSEMBL id.
#' @param biofeature String: represents either a tissue, cell type, aggregation type, protein type etc.
#'
#' @return Data frame of results from qtl credible set of variants for a given study, variant, gene and biofeature.
#'
#' @examples
#' qtl_cred_set <- qtlCredibleSet(studyid="Braineac2", variantid="1_55053079_C_T", geneid="ENSG00000169174", biofeature="SUBSTANTIA_NIGRA")
#' qtl_cred_set
#'
#'     tagVariant.id tagVariant.rsId        pval       se      beta    postProb MultisignalMethod   logABF is95 is99
#'    1_55052188_C_G              NA 1.13384e-03 0.180951  0.624280 0.001066820       conditional 1.959082 TRUE TRUE
#'    1_55052210_C_G              NA 3.01630e-04 0.180118  0.698702 0.001718659       conditional 2.435945 TRUE TRUE
#'    1_55052794_A_G              NA 1.51568e-04 0.166373 -0.681090 0.002299181       conditional 2.726953 TRUE TRUE
#'    1_55053079_C_T              NA 4.18566e-06 0.146071  0.752765 0.009346074       conditional 4.129357 TRUE TRUE
#'    ...
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
  if (grepl(pattern = "rs\\d+", variantId)) {

    # Convert rs id to variant id
    query_searchid <- "query ConvertRSIDtoVID($queryString:String!) {
    search(queryString:$queryString){
      totalVariants
      variants{
        id
        }
      }
    }"

    variables <- list(queryString = variantId)
    otg_qry$query(name = "convertid", x = query_searchid)
    id_result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$convertid, variables), flatten=TRUE)$data
    input_variantid <- id_result$search$variants$id
  }

  else if (grepl(pattern = "\\d+_\\d+_[a-zA-Z]+_[a-zA-Z]+", variantId))
  {
    input_variantid <- variantId
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

  variables <- list(studyId = studyId, variantId = input_variantid, geneId = geneId, bioFeature = bioFeature)
  otg_qry$query(name = "qtlcredset_query", x = query)

  ## Execute the query

  cli::cli_progress_step("Downloading data...", spinner = TRUE)
  qtl_cred_set <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$qtlcredset_query, variables, flatten = TRUE))$data
  df_qtl_cred <- as.data.frame(qtl_cred_set)
  base::colnames(df_qtl_cred) <- stringr::str_replace_all(colnames(df_qtl_cred),"qtlCredibleSet.","")

  return(df_qtl_cred)
}
