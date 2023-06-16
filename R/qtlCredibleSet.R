#' Retrieves QTL credible set data
#'
#' For a specific gene, lead variant, study ID, and biofeature, the function obtains tag variant information
#' that are considered to have a high probability of being truly associated with the trait and the corresponding scores.
#'
#' @param \emph{studyid} String: Open Targets Genetics generated ID for GWAS study.
#' @param \emph{variantid} String: Open Targets Genetics generated ID for variant (CHRPOSITION_REFALLELE_ALTALLELE or rsId).
#' @param \emph{gene} String: Gene ENSEMBL ID or gene name.
#' @param \emph{biofeature} String: Represents either a tissue, cell type, aggregation type, protein type, etc.
#'
#' @return Returns a data frame of results from the QTL credible set of variants consisting of the following columns:
#' \itemize{
#'   \item{\code{tagVariant.id}:} \emph{Character vector}. Tag variant ID.
#'   \item{\code{tagVariant.rsId}:} \emph{Character vector}. Tag variant rsID.
#'   \item{\code{pval}:} \emph{Numeric}. P-value.
#'   \item{\code{se}:} \emph{Numeric}. Standard error.
#'   \item{\code{beta}:} \emph{Numeric}. Beta value.
#'   \item{\code{postProb}:} \emph{Numeric}. Posterior probability.
#'   \item{\code{MultisignalMethod}:} \emph{Character vector}. Multisignal method.
#'   \item{\code{logABF}:} \emph{Numeric}. Logarithm of approximate Bayes factor.
#'   \item{\code{is95}:} \emph{Logical}. Indicates if the variant has a 95% confidence.
#'   \item{\code{is99}:} \emph{Logical}. Indicates if the variant has a 99% confidence.
#' }
#'
#' @examples
#' \dontrun{
#' result <- qtlCredibleSet(studyid = "Braineac2", variantid = "1_55053079_C_T",
#'     gene = "ENSG00000169174", biofeature = "SUBSTANTIA_NIGRA")
#' result <- qtlCredibleSet(studyid = "Braineac2", variantid = "rs7552841",
#'     gene = "PCSK9", biofeature = "SUBSTANTIA_NIGRA")
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
