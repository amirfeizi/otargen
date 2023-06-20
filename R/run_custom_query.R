#' Running custom GraphQL queries
#'
#' @param variableList is a list format which includes the key value pair list of genes/variants/study ids to be queries.
#' @param query is a GraphQL desired query body to be run.
#' @param query_name is a string format of the query name
#'
#' @importFrom magrittr %>%
#' @return a flatten json file format
#' @export
#'
#' @examples
#'\dontrun{
#' otargen::run_custom_query (variableList, query, query_name)
#'}
#'
run_custom_query <- function(variableList, query, query_name) {

  cli::cli_progress_step("Connecting the database...", spinner = TRUE)
  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()

  variables <- variableList

  otg_qry$query(name = query_name, x = query )

  ## Execute the query

  query_exec <- paste0("jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$",query_name,", variables), flatten=TRUE)$data")

  cli::cli_progress_step("Downloading data...", spinner = TRUE)
  result <- eval(parse(text = query_exec))

  return (result)
}
