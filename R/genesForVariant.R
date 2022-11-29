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


  variables <- list(variantId = variantid)

  ## Set up to query Open Targets Genetics API
  cli::cli_progress_step("Connecting to database..", spinner = TRUE)
  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()

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

  otg_qry$query(name = "v2g_query", x = query)
  cli::cli_progress_step(paste0("Downloading data for ", variantid," ..."), spinner = TRUE)

  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$v2g_query, variables), flatten=TRUE)$data

  df_result <- as.data.frame(result$genesForVariant)

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

  fin_result <- as.data.frame(df_result)

  cli::cli_progress_update()
  return (fin_result)

}
