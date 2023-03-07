#' Retrieves variants information for an input study identifier
#'
#'
#' @param studyid String: Open Targets Genetics generated id for GWAS study.
#'
#' @return A list containing all the variants information for all associated loci and information
#' about the loci genes.
#'
#' @examples
#' \dontrun{
#' otargen::studyVariants(studyid = "GCST003155")
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
