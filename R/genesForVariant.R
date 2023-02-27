#' Retrieves variant to Gene (V2G) data.
#'
#' This function gets variant ID formated as CHR_POSITION_REFALLELE_ALT_ALLELE or canonical SNPs ID as an input and returns a tibble data table including genes that are functionally implicated by this variant
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
  variables <- list(variantId = "1_154453788_C_T")


  otg_qry$query(name = "v2g_query", x = query)
  cli::cli_progress_step(paste0("Downloading data for ", variantid, " ..."), spinner = TRUE)

  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$v2g_query, variables), flatten = TRUE)$data
  result_df <- as.data.frame(result$genesForVariant) %>%
    dplyr::arrange(desc(overallScore))


 # parsing the nested json output in tidy data table format
  result_core <- result_df %>% dplyr::select(gene.symbol, variant, overallScore, gene.id )

  result_qtl <- result_df %>% dplyr::select(gene.symbol, variant, qtls) %>%
    tidyr::unnest_wider(qtls) %>%
    tidyr::unnest_longer(col = c("typeId","aggregatedScore", "tissues"), keep_empty = TRUE) %>%
    tidyr::unnest_wider(tissues) %>%
    tidyr::unnest_longer(col = c("quantile","beta","pval","tissue.id","tissue.name")
                                                             , keep_empty = TRUE)

  result_intervals <- result_df %>% dplyr::select(gene.symbol, variant, intervals) %>%
    tidyr::unnest_wider(intervals)%>%
    tidyr::unnest_longer(col = c("typeId","aggregatedScore", "tissues"), keep_empty = TRUE) %>%
    tidyr::unnest_longer( sourceId) %>%
    tidyr::unnest_wider(tissues)%>%
    tidyr::unnest_longer(col = c("quantile","score","tissue.id","tissue.name")
                         , keep_empty = TRUE)

  result_distances <- result_df %>% dplyr::select(gene.symbol, variant, distances) %>%
    tidyr::unnest_wider(distances)%>%
    tidyr::unnest_wider(tissues, names_sep = "_") %>%
    tidyr::unnest_wider(tissues_1)


  result_pkg <- list(v2g = result_core, tssd= result_distances,
                     qtls = result_qtl, chromatin = result_intervals,
                     functionalpred = result_functionalPredictions)


  cli::cli_progress_update()
  return(result_pkg)
}
