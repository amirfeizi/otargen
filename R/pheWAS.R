#' Phenome Wide Association Studies
#'
#' @param variantid
#' @export

pheWAS <- function(variantid) {
  ## Set up to query Open Targets Genetics API
  client <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  query_class <- ghql::Query$new()

  query_class$query('phewas_query' , 'query search($variantId: String!){
    pheWAS(variantId: $variantId){
    totalGWASStudies
    associations{
      pval
      beta
      oddsRatio
      study{
        studyId
        source
        pmid
        pubDate
       traitReported
        traitCategory
      }
      nTotal
    }
  }
}')

variables <- list(variantId = variantid)

result <- jsonlite::fromJSON(client$exec(query_class$queries$phewas_query, variables), flatten=TRUE)$data

result_df <- result$pheWAS %>% as.data.frame

return (result_df)

}
