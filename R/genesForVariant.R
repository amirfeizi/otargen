#' Retrieves variant-surrounding genes with calculated statistics for causal gene prioritization.
#'
#' This function retrieves all calculated prioritizing scores for surrounding genes of a specific variant based on the Open Target Genetics locus-to-gene (L2G) ML scoring pipeline.
#' It provides detailed insights into the relationship between genetic variants and genes, allowing users to explore the impact of variants on gene expression, colocalization scores, chromatin interactions, and predicted functional effects.
#' The function returns the information in a structured format, making it easier to analyze and interpret the results.
#'
#' @param variant_id A character string specifying the ID of the variant for which to fetch gene information.
#'
#' @return A list with the following components:
#' \itemize{
#'   \item \code{v2g}: A data frame with all variant-to-gene information with the following data structure:
#'     - \code{gene.symbol}: character
#'     - \code{variant}: character
#'     - \code{overallScore}: numeric
#'     - \code{gene.id}: character
#'   \item \code{tssd}: A data frame with all details on colocalization scores effect of variant in expression of the genes across tissues with the following data structure:
#'     - \code{gene.symbol}: character
#'     - \code{variant}: character
#'     - \code{typeId}: character
#'     - \code{sourceId}: character
#'     - \code{aggregatedScore}: numeric
#'     - \code{tissues_distance}: integer
#'     - \code{tissues_score}: numeric
#'     - \code{tissues_quantile}: numeric
#'     - \code{tissues_id}: character
#'     - \code{tissues_name}: character
#'   \item \code{qtls}: List of QTL associations between genes and variants across analyzed tissues with the following data structure:
#'     - \code{gene.symbol}: character
#'     - \code{variant}: character
#'     - \code{typeId}: character
#'     - \code{aggregatedScore}: numeric
#'     - \code{tissues_quantile}: numeric
#'     - \code{tissues_beta}: numeric
#'     - \code{tissues_pval}: numeric
#'     - \code{tissues_id}: character
#'     - \code{tissues_name}: character
#'   \item \code{chromatin}: A data frame including all information on chromatin interactions effect involving genes and variants with the following data structure:
#'     - \code{gene.symbol}: character
#'     - \code{variant}: character
#'     - \code{typeId}: character
#'     - \code{sourceId}: character
#'     - \code{aggregatedScore}: numeric
#'     - \code{tissues_quantile}: numeric
#'     - \code{tissues_score}: numeric
#'     - \code{tissues_id}: character
#'     - \code{tissues_name}: character
#'   \item \code{functionalpred}: A data frame including predicted functional effects of variants on genes across tissues with the following data structure:
#'     - \code{gene.symbol}: character
#'     - \code{variant}: character
#'     - \code{typeId}: character
#'     - \code{sourceId}: character
#'     - \code{aggregatedScore}: numeric
#'     - \code{tissues_maxEffectLabel}: character
#'     - \code{tissues_maxEffectScore}: numeric
#'     - \code{tissues_id}: character
#'     - \code{tissues_name}: character
#' }
#'
#' @examples
#' \dontrun{
#'   result <- genesForVariant(variant_id = "1_154453788_C_T")
#'   result <- genesForVariant(variant_id = "rs4129267")
#'   print(result)
#' }
#' @export
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom tidyr unnest
#'
#' @importFrom stringr str_replace_all
#'
#' @importFrom jsonlite fromJSON
#' @importFrom cli cli_progress_step cli_progress_update
#' @importFrom ghql GraphqlClient Query
#'

genesForVariant <- function(variant_id) {
  ## Set up to query Open Targets Genetics API
  tryCatch({
    cli::cli_progress_step("Connecting to the Open Targets Genetics GraphQL API...", spinner = TRUE)
    otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
    otg_qry <- ghql::Query$new()

    # Check variant id format
    if (grepl(pattern = "rs\\d+", variant_id)) {
      # Convert rs id to variant id
      query_searchid <- "query ConvertRSIDtoVID($queryString:String!) {
        search(queryString:$queryString){
          totalVariants
          variants{
            id
          }
        }
      }"

      variables <- list(queryString = variant_id)

      otg_qry$query(name = "convertid", x = query_searchid)
      id_result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$convertid, variables), flatten = TRUE)$data
      input_variant_id <- id_result$search$variants$id
    } else if (grepl(pattern = "\\d+_\\d+_[a-zA-Z]+_[a-zA-Z]+", variant_id)) {
      input_variant_id <- variant_id
    } else {
      stop("\nPlease provide a variant ID.")
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

    result_pkg <- list()

    variables <- list(variantId = input_variant_id)

    otg_qry$query(name = "v2g_query", x = query)
    cli::cli_progress_step(paste0("Downloading data for ", variant_id, " ..."), spinner = TRUE)

    result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$v2g_query, variables), flatten = TRUE)$data
    result_df <- as.data.frame(result$genesForVariant)

    if (nrow(result_df) != 0) {
      # parsing the nested JSON output in tidy data table format
      result_core <- result_df %>%
        dplyr::select(gene.symbol, variant, overallScore, gene.id) %>%
        dplyr::arrange(desc(overallScore))

      # qtl
      qtls_is_empty <-  all(sapply(result_df$qtls, function(x) length(x) == 0))

      if (qtls_is_empty) {
        result_qtl <- result_df %>%
          dplyr::select(gene.symbol, variant, qtls)
        result_qtl <- result_qtl %>%
          mutate(qtls = ifelse(sapply(qtls, length) == 0, NA_character_, toString(qtls)))
      } else {
        result_qtl <- result_df %>%
          dplyr::select(gene.symbol, variant, qtls) %>%
          tidyr::unnest(qtls, names_sep = '.', keep_empty = TRUE) %>%
          dplyr::rename("typeId" = "qtls.typeId",
                        "aggregatedScore" = "qtls.aggregatedScore")

        if ("qtls.tissues" %in% colnames(result_qtl)) {
          result_qtl <- result_qtl %>%
            tidyr::unnest(qtls.tissues, names_sep = '_', keep_empty = TRUE ) %>%
            dplyr::rename("tissues_id" = "qtls.tissues_tissue.id",
                          "tissues_name" = "qtls.tissues_tissue.name")
          base::colnames(result_qtl) <- stringr::str_replace_all(colnames(result_qtl), "qtls.", "")
        }
      }

      # intervals
      ints_is_empty <-  all(sapply(result_df$intervals, function(x) length(x) == 0))

      if (ints_is_empty) {
        result_intervals <- result_df %>%
          dplyr::select(gene.symbol, variant, intervals)
        result_intervals <- result_intervals %>%
          mutate(intervals = ifelse(sapply(intervals, length) == 0, NA_character_, toString(intervals)))
      } else {
        result_intervals <- result_df %>%
          dplyr::select(gene.symbol, variant, intervals) %>%
          tidyr::unnest(intervals, names_sep = '.', keep_empty = TRUE) %>%
          dplyr::rename("typeId" = "intervals.typeId",
                        "aggregatedScore" = "intervals.aggregatedScore")

        if ("intervals.tissues" %in% colnames(result_intervals)) {
          result_intervals <- result_intervals %>%
            tidyr::unnest(intervals.tissues, names_sep = '_', keep_empty = TRUE) %>%
            dplyr::rename("tissues_id" = "intervals.tissues_tissue.id",
                          "tissues_name" = "intervals.tissues_tissue.name")
          base::colnames(result_intervals) <- stringr::str_replace_all(colnames(result_intervals), "intervals.", "")
        }
      }

      # distances
      dists_is_empty <- all(sapply(result_df$distances, function(x) length(x) == 0))

      if (dists_is_empty) {
        result_distances <- result_df %>%
          dplyr::select(gene.symbol, variant, distances)
        result_distances <- result_distances %>%
          mutate(distances = ifelse(sapply(distances, length) == 0, NA_character_, toString(distances)))
      } else {
        result_distances <- result_df %>%
          dplyr::select(gene.symbol, variant, distances) %>%
          tidyr::unnest(distances, names_sep = '.', keep_empty = TRUE) %>%
          dplyr::rename("typeId" = "distances.typeId",
                        "aggregatedScore" = "distances.aggregatedScore")

        if ("distances.tissues" %in% colnames(result_distances)) {
          result_distances <- result_distances %>%
            tidyr::unnest(distances.tissues, names_sep = '_', keep_empty = TRUE) %>%
            dplyr::rename("tissues_id" = "distances.tissues_tissue.id",
                          "tissues_name" = "distances.tissues_tissue.name")
          base::colnames(result_distances) <- stringr::str_replace_all(colnames(result_distances), "distances.", "")
        }
      }

      # result_functionalPredictions
      funcPreds_is_empty <- all(sapply(result_df$functionalPredictions, function(x) length(x) == 0))

      if (funcPreds_is_empty) {
        result_functionalPredictions <- result_df %>%
          dplyr::select(gene.symbol, variant, functionalPredictions)
        result_functionalPredictions <- result_functionalPredictions %>%
          mutate(functionalPredictions = ifelse(sapply(functionalPredictions, length) == 0, NA_character_, toString(functionalPredictions)))
      } else {
        result_functionalPredictions <- result_df %>%
          dplyr::select(gene.symbol, variant, functionalPredictions) %>%
          tidyr::unnest(functionalPredictions, names_sep = '.', keep_empty = TRUE) %>%
          dplyr::rename("typeId" = "functionalPredictions.typeId",
                        "aggregatedScore" = "functionalPredictions.aggregatedScore")

        if ("functionalPredictions.tissues" %in% colnames(result_functionalPredictions)) {
          result_functionalPredictions <- result_functionalPredictions %>%
            tidyr::unnest(functionalPredictions.tissues, names_sep = '_', keep_empty = TRUE) %>%
            dplyr::rename("tissues_id" = "functionalPredictions.tissues_tissue.id",
                          "tissues_name" = "functionalPredictions.tissues_tissue.name")
          base::colnames(result_functionalPredictions) <- stringr::str_replace_all(colnames(result_functionalPredictions), "functionalPredictions.", "")
        }
      }

      result_pkg <- list(v2g = result_core, tssd = result_distances,
                         qtls = result_qtl, chromatin = result_intervals,
                         functionalpred = result_functionalPredictions)
    }
    cli::cli_progress_update()
    return(result_pkg)

  }, error = function(e) {
    # Handling connection timeout
    if(grepl("Timeout was reached", e$message)) {
      stop("Connection timeout reached while connecting to the Open Targets Genetics GraphQL API.")
    } else {
      stop(e) # Handle other types of errors
    }
  })
}
