#' Retrieves variant to Gene (V2G) data.
#'
#' This function gets variant ID formated as CHR_POSITION_REFALLELE_ALT_ALLELE or canonical SNPs ID as an input and returns a list of tibble data tables including ranked gene table and corresponding functional genomics data tables that are functionally implicated by this variant
#'
#' @param variantid String: Open Target Genetics generated id for variant (CHR_POSITION_REFALLELE_ALT_ALLELE or rsId).
#'
#' @return A list of tibble data tables containing variant to gene information with
#' individual QTL associations, intervals, distances and functional predictions.
#'
#' @examples
#' \dontrun{
#' otargen::genesForVariant(variantid = "1_154453788_C_T")
#' otargen::genesForVariant(variantid = "rs4129267")
#' }
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

  result_pkg = list()

  variables <- list(variantId = input_variantid)
  variables <- list(variantId = "1_154453788_C_T")


  otg_qry$query(name = "v2g_query", x = query)
  cli::cli_progress_step(paste0("Downloading data for ", variantid, " ..."), spinner = TRUE)

  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$v2g_query, variables), flatten = TRUE)$data
  result_df <- as.data.frame(result$genesForVariant) %>%
    dplyr::arrange(desc(overallScore))

  if (nrow(result_df)!=0){
 # parsing the nested json output in tidy data table format
  result_core <- result_df %>% dplyr::select(gene.symbol, variant, overallScore, gene.id )


  #qtl
  result_qtl <- result_df %>% dplyr::select(gene.symbol, variant, qtls) %>%
                tidyr::unnest(qtls, names_sep='.', keep_empty=TRUE) %>% dplyr::rename("typeId"="qtls.typeId", "aggregatedScore"
                                                                                                 ="qtls.aggregatedScore")
  if ("qtls.tissues" %in% colnames(result_qtl)){
    result_qtl <- result_qtl %>% tidyr::unnest(qtls.tissues, names_sep='_',keep_empty=TRUE ) %>%
                  dplyr::rename("tissues_id"="qtls.tissues_tissue.id", "tissues_name"="qtls.tissues_tissue.name")
    base::colnames(result_qtl) <- stringr::str_replace_all(colnames(result_qtl),"qtls.","")
  }


  #intervals
  result_intervals <- result_df %>% dplyr::select(gene.symbol, variant, intervals) %>%
                      tidyr::unnest(intervals, names_sep='.', keep_empty=TRUE) %>% dplyr::rename("typeId"="intervals.typeId", "aggregatedScore"
                                                                          ="intervals.aggregatedScore")
  if ("intervals.tissues" %in% colnames(result_intervals)){
    result_intervals <- result_intervals %>% tidyr::unnest(intervals.tissues, names_sep='_',keep_empty=TRUE )%>%
                        dplyr::rename("tissues_id"="intervals.tissues_tissue.id", "tissues_name"="intervals.tissues_tissue.name")
    base::colnames(result_intervals) <- stringr::str_replace_all(colnames(result_intervals),"intervals.","")
  }


  #distances
  result_distances <- result_df %>% dplyr::select(gene.symbol, variant, distances) %>%
                      tidyr::unnest(distances, names_sep='.', keep_empty=TRUE) %>% dplyr::rename("typeId"="distances.typeId", "aggregatedScore"
                                                                               ="distances.aggregatedScore")
  if ("distances.tissues" %in% colnames(result_distances)){
    result_distances <- result_distances %>% tidyr::unnest(distances.tissues, names_sep='_',keep_empty=TRUE )%>%
                        dplyr::rename("tissues_id"="distances.tissues_tissue.id", "tissues_name"="distances.tissues_tissue.name")
    base::colnames(result_distances) <- stringr::str_replace_all(colnames(result_distances),"distances.","")
  }


  #result_functionalPredictions
  result_functionalPredictions <- result_df %>% dplyr::select(gene.symbol, variant, functionalPredictions) %>%
    tidyr::unnest(functionalPredictions, names_sep='.', keep_empty=TRUE) %>% dplyr::rename("typeId"="functionalPredictions.typeId", "aggregatedScore"
                                                                               ="functionalPredictions.aggregatedScore")
  if ("functionalPredictions.tissues" %in% colnames(result_functionalPredictions)){
    result_functionalPredictions <- result_functionalPredictions %>% tidyr::unnest(functionalPredictions.tissues, names_sep='_',keep_empty=TRUE )%>%
                                    dplyr::rename("tissues_id"="functionalPredictions.tissues_tissue.id", "tissues_name"="functionalPredictions.tissues_tissue.name")
    base::colnames(result_functionalPredictions) <- stringr::str_replace_all(colnames(result_functionalPredictions),"functionalPredictions.","")
  }



  result_pkg <- list(v2g = result_core, tssd= result_distances,
                     qtls = result_qtl, chromatin = result_intervals,
                     functionalpred = result_functionalPredictions)

  }
  cli::cli_progress_update()
  return(result_pkg)
}
