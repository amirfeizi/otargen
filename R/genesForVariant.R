#' Variant to Gene (V2G) data
#'
#'
#' @param variantid is the Open Target Genetics generated id for each variants.
#' @return A dataframe containing variant to gene (v2g) information with individual QTL associations, distances and functional predictions.
#' @examples
#' genesForVariant("1_55039974_G_T")
#' @export
#'

genesForVariant <- function(variantid) {

  ## Set up to query Open Targets Genetics API
  cli::cli_progress_step("Connecting to database..", spinner = TRUE)
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


  query <- "query v2gquery($variantId: String!){
  genesForVariant(variantId: $variantId) {
    gene{
    id
    symbol
  }
  overallScore
  qtls{
    typeId
    aggregatedScore
    tissues{
      tissue{
        id
        name
      }
      beta
      pval
    }
  }
  functionalPredictions{
    typeId
    sourceId
    aggregatedScore
    tissues{
      tissue{
        id
        name
      }
      maxEffectLabel
      maxEffectScore
    }
  }
  distances{
    typeId
    sourceId
    aggregatedScore
    tissues{
      tissue{
        id
        name
      }
    }
  }

  }
}"

  ## Execute the query


  variables <- list(variantId = input_variantid)

  otg_qry$query(name = "v2g_query", x = query)
  cli::cli_progress_step(paste0("Downloading data for ", variantid," ..."), spinner = TRUE)

  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$v2g_query, variables), flatten=TRUE)$data

  df_result <- as.data.frame(result$genesForVariant)
  if (nrow(df_result)!=0){
  df_result <- tidyr::unnest(df_result, qtls, names_sep='.', keep_empty=TRUE)
    if ("qtls.tissues" %in% colnames(df_result)){
      df_result <- tidyr::unnest(df_result, qtls.tissues, names_sep='.',keep_empty=TRUE )
    }

  df_result <- tidyr::unnest(df_result, functionalPredictions, names_sep='.', keep_empty=TRUE)
    if ("functionalPredictions.tissues" %in% colnames(df_result)){
      df_result <- tidyr::unnest(df_result, functionalPredictions.tissues, names_sep='.', keep_empty=TRUE)
    }

  df_result <- tidyr::unnest(df_result, distances, names_sep='.', keep_empty=TRUE)
    if ("distances.tissues" %in% colnames(df_result)){
      df_result <- tidyr::unnest(df_result, distances.tissues, names_sep='.',keep_empty=TRUE)
    }
}
  fin_result <- as.data.frame(df_result)

  cli::cli_progress_update()
  return (fin_result)

}
