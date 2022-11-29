#' Get gwas colocalisation data for a given region
#'
#' @param chromosome chromosome number given as string.
#' @param start start position of the specified chromosome.
#' @param end end position of the specified chromosome.
#' @returns A data frame with gwas colocalisation data containing studies and variants for the queried chromosome and region
#' @examples
#' gwasColocalisationForRegion("1", 153992685, 154155116)
#' @export
#'
#'

gwasColocalisationForRegion <- function(chromosome, start, end) {

  ## Set up to query Open Targets Genetics API
  variables <- list(chromosome = chromosome, start = start, end = end)

  cli::cli_progress_step("Connecting the dataase...", spinner = TRUE)
  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()

  query <- "query gwascolforregquery($chromosome: String!, $start: Long!, $end: Long!){
  gwasColocalisationForRegion(chromosome: $chromosome, start: $start, end: $end) {
    leftVariant{
      id
    position
    chromosome
      rsId
    }
  leftStudy{
    studyId
    traitReported
  }
  rightVariant
  {
    id
    position
    chromosome
    rsId
  }
  rightStudy
  {
    studyId
    traitCategory
  }
  h3
  h4
  log2h4h3
  }
}"

  ## Execute the query
  otg_qry$query(name = "gwascolforreg_query", x = query)

  cli::cli_progress_step("Downloading the data...", spinner = TRUE)
  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$gwascolforreg_query, variables, flatten=TRUE))$data

  result_df <- result$gwasColocalisationForRegion %>% as.data.frame

  return(result_df)
}
