#' Get qtl credible set data
#'
#' @param studyid is the Open Target Genetics generated id for gwas studies.
#' @param variantid is Open Target Genetics generated id for each variant in the databse.
#' @param geneid is the Ensembl identifier.
#' @param biofeature represents either a tissue, cell type, aggregation type, protein type etc.
#' @return A data frame of results from qtl credible set of variants for a given study, variant, gene and biofeature.
#' @examples
#' qtlCredibleSet("Braineac2", "1_55053079_C_T", "ENSG00000169174", "SUBSTANTIA_NIGRA")
#' @export
#'

qtlCredibleSet <- function(studyId, variantId, geneId, bioFeature) {

  ## Set up to query Open Targets Genetics API

  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()
  otg_qry$query("qtlcredset_query", "query qtlcredsetquery($studyId: String!, $variantId: String!, $geneId: String!, $bioFeature: String!){
  qtlCredibleSet(studyId: $studyId, variantId: $variantId, geneId: $geneId, bioFeature: $bioFeature) {
  tagVariant {
      id
    }
  pval
  beta
  postProb
  MultisignalMethod
  logABF
  is95
  is99
}
}")

  ## Execute the query
  variables <- list(studyId = studyId, variantId = variantId, geneId = geneId, bioFeature = bioFeature)
  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$qtlcredset_query, variables, flatten = TRUE))$data
  result <- as.data.frame(result)

  return(result)
}
