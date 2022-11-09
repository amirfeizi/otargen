#' Genes and Variants association with
#'
#' @export
#'

genesForVariant <- function(variantid) {

  ## Set up to query Open Targets Genetics API

  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()
  otg_qry$query("v2g_query", "query v2gquery($variantId: String!){
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
}")

  ## Execute the query
  variables <- list(variantId = variantid)

  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$v2g_query, variables), flatten=TRUE)$data

  main_result <- as.data.frame(result$genesForVariant)
  df_result <- tidyr::unnest(main_result, qtls, names_sep='.', keep_empty=TRUE)
  df_result <- tidyr::unnest(df_result, qtls.tissues, names_sep='.',keep_empty=TRUE )
  df_result <- tidyr::unnest(df_result, functionalPredictions, names_sep='.', keep_empty=TRUE)
  df_result <- tidyr::unnest(df_result, functionalPredictions.tissues, names_sep='.', keep_empty=TRUE)
  df_result <- tidyr::unnest(df_result, distances, names_sep='.', keep_empty=TRUE)
  df_result <- tidyr::unnest(df_result, distances.tissues, names_sep='.',keep_empty=TRUE)
  fin_result <- as.data.frame(df_result)
  return(fin_result)

}
