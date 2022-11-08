#' G2V
#'
#' @export

genesForVariant <- function(variantid) {


  variables <- list(variantId = variantid)

  ## Set up to query Open Targets Genetics API

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
