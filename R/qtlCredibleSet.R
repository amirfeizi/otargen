#' Get qtl credible set data
#'
#' @param studyid is the Open Target Genetics generated id for gwas studies.
#' @param variantid is Open Target Genetics generated id for each variant in the databse.
#' @param geneid is the Ensembl identifier.
#' @param biofeature represents either a tissue, cell type, aggregation type, protein type etc.
#' @export

qtlCredibleSet <- function(studyid, variantid, geneid, biofeature) {

  ## Set up to query Open Targets Genetics API

  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()
  otg_qry$query("qtlcredset_query", "query qtlcredsetquery($studyId: String!, $variantId: String!, $geneId: String!, $bioFeature: String!){
  qtlCredibleSet(studyId: $studyId, variantId: $variantId, geneId: $geneId, bioFeature: $bioFeature) {
  tagVariant {
      chromosome
      position
      refAllele
      altAllele
      rsId
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
  variables <- list(studyId = studyid, variantId = variantid, geneId = geneid, bioFeature = biofeature)
  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$qtlcredset_query, variables, flatten = TRUE))$data

  return(result)
}
