#' Retrieves variants information for a study.
#'
#' For an input study ID, this function provides information on all variants associated with all loci
#' and information about the genes within the loci.
#'
#' @param \emph{studyid} String: Open Targets Genetics generated ID for a GWAS study.
#'
#' @return Returns a list of two data frames.
#'
#' the first data frame (tibble format) includes the loci data frame with following data structure:
#' \itemize{
#'   \item{\code{variant.id}:} \emph{Character}. Variant ID.
#'   \item{\code{pval}:} \emph{Numeric}. P-value.
#'   \item{\code{variant.nearestCodingGene.symbol}:} \emph{Character}. Symbol of the nearest coding gene to the variant.
#'   \item{\code{variant.rsId}:} \emph{Character}. Variant rsID.
#'   \item{\code{variant.chromosome}:} \emph{Character}. Chromosome of the variant.
#'   \item{\code{variant.position}:} \emph{Integer}. Position of the variant.
#'   \item{\code{variant.nearestCodingGeneDistance}:} \emph{Integer}. Distance to the nearest coding gene.
#'   \item{\code{credibleSetSize}:} \emph{Integer}. Size of the credible set.
#'   \item{\code{ldSetSize}:} \emph{Integer}. Size of the LD set.
#'   \item{\code{oddsRatio}:} \emph{Numeric}. Odds ratio.
#'   \item{\code{beta}:} \emph{Numeric}. Beta value.
#' }
#'
#' The second data frame includes gene information with following data structure:
#' \itemize{
#'   \item{\code{score}:} \emph{Numeric}. Gene score.
#'   \item{\code{gene.id}:} \emph{Character}. Gene ID.
#'   \item{\code{gene.symbol}:} \emph{Character}. Gene symbol.
#' }
#'
#' @examples
#' \dontrun{
#' result <- studyVariants(studyid = "GCST003155")
#'}
#' @importFrom magrittr %>%
#' @export
#'

studyVariants <- function(studyid) {
  ## Set up to query Open Targets Genetics API
  variables <- list(studyId = studyid)
  # variables <- list(studyId = "GCST003155")

  cli::cli_progress_step("Connecting the database...", spinner = TRUE)
  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()


  query <- "query StudyVariants($studyId: String!) {
  manhattan(studyId: $studyId) {
    associations {
      variant {
        id
        rsId
        chromosome
        position
        nearestCodingGene {
          id
          symbol

        }
        nearestCodingGeneDistance

      }
      pval
      credibleSetSize
      ldSetSize
      oddsRatio
      oddsRatioCILower
      oddsRatioCIUpper
      beta
      betaCILower
      betaCIUpper
      direction
      bestGenes {
        score
        gene {
          id
          symbol

        }

      }
    }
  }
}"


  ## Execute the query

  otg_qry$query(name = "StudyVariants", x = query)

  cli::cli_progress_step("Downloading data...", spinner = TRUE)
  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$StudyVariants, variables), flatten = TRUE)$data
  loci_all <- as.data.frame(result$manhattan$associations) %>% dplyr::arrange(pval)
  loci_all_core <- loci_all %>% dplyr::select(-bestGenes)
  loci_all_core <- loci_all_core[, c(11, 1, 17, 12:15, 2:4, 7, 10)]
  loci_all_best_genes <- loci_all$bestGenes

  final_output <- list(loci_data = loci_all_core, loci_genes = loci_all_best_genes)



  if (nrow(final_output$loci_data) == 0) {
    final_output <- data.frame()
  }
  return(final_output)
}
