#' GWAS Regional Association of a study.
#'
#' with providing a study id and a chromosomal regin information,
#' this function returns a tibble data table with all variants and
#'  their respective p-value as shown in the example.
#'
#' @param studyid String: Open Target Genetics generated id for GWAS study.
#' @param chromosome String: chromosome number as string.
#' @param start Long: start position of the specified chromosome.
#' @param end Long: end position of the specified chromosome.
#'
#'
#' @return Data frame of variants and p-val.
#'
#' @examples
#' gwas_reg <- gwasRegional(studyid="GCST90002357", chromosome="1", start=153992685, end=154155116)
#' gwas_reg
#'
#'        pval      variant.id variant.chromosome variant.position
#'   5.000e-20 1_153993605_G_C                  1        153993605
#'   1.580e-06 1_153993862_C_T                  1        153993862
#'   2.750e-17 1_153994184_T_G                  1        153994184
#'
#' @export
#'
#'

gwasRegional <- function(studyid, chromosome, start, end) {

  ## Set up to query Open Targets Genetics API

  variables <- list(studyId = studyid, chromosome = chromosome, start = start, end = end)

  cli::cli_progress_step("Connecting the database...", spinner = TRUE)
  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()

  query <- "query gwasregionalquery($studyId: String!, $chromosome: String! $start: Long!, $end: Long!){
  gwasRegional(studyId: $studyId, chromosome: $chromosome, start: $start, end: $end) {
     variant{
    id
    chromosome
    position
  }
  pval

  }
}"


  ## Execute the query

  otg_qry$query(name = "gwasregional_query", x =  query)

  cli::cli_progress_step("Downloading data...", spinner = TRUE)
  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$gwasregional_query, variables), flatten=TRUE)$data

  output <- result$gwasRegional %>% as.data.frame

  return(output)
}
