#' Retrieve GWAS summary statistics to create a Manhattan plot
#'
#' This function retrieves summary statistics for a given GWAS study ID, which are used to generate a Manhattan plot.
#' The Manhattan plot is a graphical representation of genetic association studies, particularly in genome-wide association studies (GWAS).
#' It displays the results of statistical associations between genetic variants and a trait or disease of interest across the genome.
#' This function returns a data frame of the underlying data, which can be used to recreate the Manhattan plot using the \code{plot_manhattan()} function,
#' or for custom plots and downstream analysis.
#'
#' @param study_id Character: Open Targets Genetics generated ID for the GWAS study.
#' @param pageindex Int: Index of the current page (pagination index >= 0).
#' @param pagesize Int: Number of records in a page (pagination size > 0).
#'
#' @return Returns a data frame containing the Manhattan associations for the input study ID. The table consists of the following columns:
#' \itemize{
#'   \item{\code{pval_mantissa}:} Numeric vector. Mantissa of the p-value.
#'   \item{\code{pval_exponent}:} Integer vector. Exponent of the p-value.
#'   \item{\code{credible_set_size}:} Integer vector. Size of the credible set.
#'   \item{\code{ld_set_size}:} Integer vector. Size of the LD set.
#'   \item{\code{total_set_size}:} Integer vector. Total size of the set.
#'   \item{\code{pval}:} Numeric vector. P-value.
#'   \item{\code{odds_ratio}:} Logical vector. Odds ratio.
#'   \item{\code{odds_ratio_ci_lower}:} Logical vector. Lower confidence interval of the odds ratio.
#'   \item{\code{odds_ratio_ci_upper}:} Logical vector. Upper confidence interval of the odds ratio.
#'   \item{\code{beta}:} Numeric vector. Beta value.
#'   \item{\code{beta_ci_lower}:} Numeric vector. Lower confidence interval of the beta value.
#'   \item{\code{beta_ci_upper}:} Numeric vector. Upper confidence interval of the beta value.
#'   \item{\code{direction}:} Character vector. Direction of the effect.
#'   \item{\code{best_genes_score}:} Numeric vector. Score of the best genes.
#'   \item{\code{best_genes_gene_id}:} Character vector. Gene ID of the best genes.
#'   \item{\code{best_genes_gene_symbol}:} Character vector. Gene symbol of the best genes.
#'   \item{\code{best_coloc_genes_score}:} Numeric vector. Score of the best colocated genes.
#'   \item{\code{best_coloc_genes_gene_id}:} Character vector. Gene ID of the best colocated genes.
#'   \item{\code{best_coloc_genes_gene_symbol}:} Character vector. Gene symbol of the best colocated genes.
#'   \item{\code{best_locus2genes_score}:} Numeric vector. Score of the best locus-to-genes.
#'   \item{\code{best_locus2genes_gene_id}:} Character vector. Gene ID of the best locus-to-genes.
#'   \item{\code{best_locus2genes_gene_symbol}:} Character vector. Gene symbol of the best locus-to-genes.
#'   \item{\code{variant_id}:} Character vector. Variant ID.
#'   \item{\code{variant_position}:} Integer vector. Variant position.
#'   \item{\code{variant_chromosome}:} Character vector. Variant chromosome.
#'   \item{\code{variant_rs_id}:} Character vector. Variant rsID.
#' }
#'
#' @examples
#' \dontrun{
#' result <- manhattan(study_id = "GCST90002357")
#' result <- manhattan(study_id = "GCST90002357", pageindex = 2, pagesize = 50)
#' }
#' @import dplyr
#' @importFrom magrittr %>%
#' @export
#'
#'
manhattan <- function(study_id, pageindex = 0, pagesize = 100) {
  ## Set up to query Open Targets Genetics API

tryCatch({
  cli::cli_progress_step("Connecting to the Open Targets Genetics GrpahQL API...", spinner = TRUE)
  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()

  variables <- list(studyId = study_id, pageIndex = pageindex, pageSize = pagesize)

  query <- "query manhattanquery($studyId: String!, $pageIndex: Int!, $pageSize: Int!) {
    manhattan(studyId: $studyId, pageIndex: $pageIndex, pageSize: $pageSize) {
      associations {
        pvalMantissa
        pvalExponent
        credibleSetSize
        ldSetSize
        totalSetSize
        variant {
          id
          position
          chromosome
          rsId
        }
        pval
        oddsRatio
        oddsRatioCILower
        oddsRatioCIUpper
        beta
        betaCILower
        betaCIUpper
        direction
        bestGenes {
          gene {
            id
            symbol
          }
          score
        }
        bestColocGenes {
          gene {
            id
            symbol
          }
          score
        }
        bestLocus2Genes {
          gene {
            id
            symbol
          }
          score
        }
      }
    }
  }"

  ## Execute the query
  otg_qry$query(name = "manhattan_query", x = query)

  cli::cli_progress_step("Downloading data...", spinner = TRUE)
  man_result <- jsonlite::fromJSON(
    otg_cli$exec(otg_qry$queries$manhattan_query, variables),
    flatten = TRUE
  )$data
  man_result <- as.data.frame(man_result$manhattan$associations)

  if (nrow(man_result) != 0) {
    man_result <- tidyr::unnest(man_result, bestGenes, names_sep = ".", keep_empty = TRUE)
    man_result <- tidyr::unnest(man_result, bestColocGenes, names_sep = ".", keep_empty = TRUE)
    man_result <- tidyr::unnest(man_result, bestLocus2Genes, names_sep = ".", keep_empty = TRUE)

    man_result <- janitor::clean_names(man_result)
  } else {
    man_result <- data.frame()
  }

  return(man_result)

}, error = function(e) {
  # Handling connection timeout
  if(grepl("Timeout was reached", e$message)) {
    stop("Connection timeout reached while connecting to the Open Targets Genetics GraphQL API.")
  } else {
    stop(e) # Handle other types of errors
  }
})
}
