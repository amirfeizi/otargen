#' Retrieves QTL credible set data
#'
#' For a specific gene, lead variant, study id and biofeature, the function obtains tag variant information
#' that are considered to have a high probability of being truly associated with the trait and the corresponding scores.
#'
#' @param \emph{studyid} String: Open Target Genetics generated id for GWAS study.
#' @param \emph{variantid} String: Open Target Genetics generated id for variant (CHRPOSITION_REFALLELE_ALTALLELE or rsId).
#' @param \emph{gene} String: a gene ENSEMBL id or a gene name.
#' @param \emph{biofeature} String: represents either a tissue, cell type, aggregation type, protein type etc.
#'
#' @return Returns a data frame of results from qtl credible set of variants consisting of the following columns:
#'
#' \enumerate{
#' \item tagVariant.id
#' \item tagVariant.rsId
#' \item pval
#' \item se
#' \item beta
#' \item postProb
#' \item MultisignalMethod
#' \item logABF
#' \item is95
#' \item is99
#' }
#'
#' @examples
#' \dontrun{
#' otargen::qtlCredibleSet(studyid="Braineac2", variantid="1_55053079_C_T",
#'     gene="ENSG00000169174", biofeature="SUBSTANTIA_NIGRA")
#' otargen::qtlCredibleSet(studyid="Braineac2", variantid="rs7552841",
#'     gene="PCSK9", biofeature="SUBSTANTIA_NIGRA")
#'}
#'
#' @importFrom magrittr %>%
#' @export
#'
#'
qtlCredibleSet <- function(studyid, variantid, gene, biofeature) {
  ## Set up to query Open Targets Genetics API
  cli::cli_progress_step("Connecting the database...", spinner = TRUE)
  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()

  # Check gene format
  #Query for gene name search
  # check for gene name:
  query_search <- "query convertnametoid($queryString:String!) {
    search(queryString:$queryString){
      genes{
        id
        symbol
      }
      }
    }"

  if (!grepl(pattern = "ENSG\\d{11}", gene)){
    variables <- list(queryString = gene)
    otg_qry$query(name = "convertnametoid", x = query_search)
    id_result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$convertnametoid, variables), flatten = TRUE)$data
    id <- as.data.frame(id_result$search$genes)
    if (nrow(id)!=0){
      name_match <- id[id$symbol == gene, ]
      gene_input <- name_match$id
    } else{
      stop ("\nPlease provide Ensemble gene ID or gene name")
    }
  } else{
    gene_input <- gene
  }


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

  variables <- list(studyId = studyid, variantId = input_variantid, geneId = gene_input, bioFeature = biofeature)
  otg_qry$query(name = "qtlcredset_query", x = query)

  ## Execute the query

  cli::cli_progress_step("Downloading data...", spinner = TRUE)
  qtl_cred_set <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$qtlcredset_query, variables, flatten = TRUE))$data
  df_qtl_cred <- as.data.frame(qtl_cred_set)
  base::colnames(df_qtl_cred) <- stringr::str_replace_all(colnames(df_qtl_cred),"qtlCredibleSet.","")

  return(df_qtl_cred)
}
