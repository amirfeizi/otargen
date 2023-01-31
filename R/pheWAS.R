#' Phenome Wide Association Studies
#'
#' @param variantid is the Open Target Genetics generated id for each variants
#' @returns A data frame with PheWAS associations in it
#' @examples
#' pheWAS("1_154549918_C_A")
#' pheWAS("1_154562290_G_A")
#' @export
#'

pheWAS <- function(variantid) {
  ## Set up to query Open Targets Genetics API

  variables <- list(variantId = variantid)
  query <- 'query search($variantId: String!){
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
}'

  # make connection to the endpoint

  cli::cli_progress_step("Connecting the database...", spinner = TRUE)
  client <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  query_class <- ghql::Query$new()

  # execute the query
  query_class$query(name ='phewas_query' , x = query )

  cli::cli_progress_step(paste("Downloading data...",variantid,"..."), spinner = TRUE)

  result <- jsonlite::fromJSON(client$exec(query_class$queries$phewas_query, variables), flatten=TRUE)$data
  result_df <- data.frame()
  if (length(result$pheWAS$associations)!=0){
    result_df <- result$pheWAS %>% as.data.frame
  }

  base::colnames(result_df) <- stringr::str_replace_all(colnames(result_df),"associations.","")
  return (result_df)
}
