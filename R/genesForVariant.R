#' Retrieves variant to Gene (V2G) data.
#'
#' This function gets Variant ID formated as CHR_POSITION_REFALLELE_ALT_ALLELE or canonical SNPs id as an input and returns a tibble data table containing the following columns-
#' Variant, overallScore, qtls.typeId, qtls.aggregatedScore,
#' qtls.tissues.quantile, qtls.tissues.beta, qtls.tissues.pval,
#' qtls.tissues.id, qtls.tissues.name, int.typeId,
#' int.sourceId, int.aggregatedScore, int.tissues.quantile,
#' int.tissues.score, int.tissues.id, intervals.tissues.name, funcPreds.typeId,
#' funcPreds.sourceId, funcPreds.aggregatedScore, funcPreds.tissues.maxEffectLabel,
#' funcPreds.tissues.maxEffectScore, functionalPreds.tissues.id, funcPreds.tissues.name,
#' dist.typeId, dist.sourceId, dist.aggregatedScore, dist.tissues.distance,
#' dist.tissues.score, dist.tissues.quantile, dist.tissues.id,
#' dist.tissues.name, gene.id, and gene.symbol.
#'
#' @param variantid String: Open Target Genetics generated id for variant (CHR_POSITION_REFALLELE_ALT_ALLELE or rsId).
#'
#' @return Data frame containing variant to gene information with
#' individual QTL associations, intervals, distances and functional predictions.
#'
#' @examples
#' genesForVariant(variantid = "1_154453788_C_T")
#' or
#' genesForVariant(variantid = "rs4129267")
#'
#' @export
#'
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
    id_result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$convertid, variables), flatten = TRUE)$data
    input_variantid <- id_result$search$variants$id
  } else if (grepl(pattern = "\\d+_\\d+_[a-zA-Z]+_[a-zA-Z]+", variantid)) {
    input_variantid <- variantid
  } else {
    stop("\n Please provide a variant Id")
  }


  query <- "query v2gquery($variantId: String!){
  genesForVariant(variantId: $variantId) {
    gene{
    id
    symbol
    }
  variant
  overallScore
  qtls{
    typeId
    aggregatedScore
    tissues{
      tissue{
        id
        name
      }
      quantile
      beta
      pval
    }
  }
  intervals{
  typeId
  sourceId
  aggregatedScore
  tissues{
  tissue{
  id
  name
  }
  quantile
  score
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
      distance
      score
      quantile
    }
  }
  }
}"

  ## Execute the query


  variables <- list(variantId = input_variantid)

  otg_qry$query(name = "v2g_query", x = query)
  cli::cli_progress_step(paste0("Downloading data for ", variantid, " ..."), spinner = TRUE)

  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$v2g_query, variables), flatten = TRUE)$data
  df_result <- as.data.frame(result$genesForVariant)
  if (nrow(df_result) != 0) {
    df_result <- tidyr::unnest(df_result, qtls, names_sep = ".", keep_empty = TRUE)
    if ("qtls.tissues" %in% colnames(df_result)) {
      df_result <- tidyr::unnest(df_result, qtls.tissues, names_sep = ".", keep_empty = TRUE)
    }

    df_result <- tidyr::unnest(df_result, functionalPredictions, names_sep = ".", keep_empty = TRUE)
    if ("functionalPredictions.tissues" %in% colnames(df_result)) {
      df_result <- tidyr::unnest(df_result, functionalPredictions.tissues, names_sep = ".", keep_empty = TRUE)
    }

    df_result <- tidyr::unnest(df_result, intervals, names_sep = ".", keep_empty = TRUE)
    if ("intervals.tissues" %in% colnames(df_result)) {
      df_result <- tidyr::unnest(df_result, intervals.tissues, names_sep = ".", keep_empty = TRUE)
    }

    df_result <- tidyr::unnest(df_result, distances, names_sep = ".", keep_empty = TRUE)
    if ("distances.tissues" %in% colnames(df_result)) {
      df_result <- tidyr::unnest(df_result, distances.tissues, names_sep = ".", keep_empty = TRUE)
    }

    colnames(df_result) <- c(
      "Variant", "overallScore", "qtls.typeId", "qtls.aggregatedScore",
      "qtls.tissues.quantile", "qtls.tissues.beta", "qtls.tissues.pval",
      "qtls.tissues.id", "qtls.tissues.name", "int.typeId",
      "int.sourceId", "int.aggregatedScore", "int.tissues.quantile",
      "int.tissues.score", "int.tissues.id", "int.tissues.name", "funcPreds.typeId",
      "funcPreds.sourceId", "funcPreds.aggregatedScore", "funcPreds.tissues.maxEffectLabel",
      "funcPreds.tissues.maxEffectScore", "funcPreds.tissues.id", "funcPreds.tissues.name",
      "dist.typeId", "dist.sourceId", "dist.aggregatedScore", "dist.tissues.distance",
      "dist.tissues.score", "dist.tissues.quantile", "distances.tissues.id",
      "dist.tissues.name", "gene.id", "gene.symbol"
    )
  }
  fin_result <- as.data.frame(df_result)

  cli::cli_progress_update()
  return(fin_result)
}
