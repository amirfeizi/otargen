#' Retrieves GWAS study id information.
#'
#' For a given study id, this function returns all the  relevant information about
#' GWAS study such as PubMed id, studied trait EFO id, case/control size etc.
#'
#' @param studyid String: Open Targets Genetics generated id for a GWAS study.
#'
#' @return Returns a tibble data table containing the GWAS study information. The columns in the table are as follows:
#'
#' \enumerate{
#' \item studyId
#' \item traitReported
#' \item source
#' \item traitEfos
#' \item pmid
#' \item pubDate
#' \item pubJournal
#' \item pubTitle
#' \item pubAuthor
#' \item hasSumstats
#' \item ancestryInitial
#' \item nInitial
#' \item nReplication
#' \item traitCategory
#' \item numAssocLoci
#' \item nTotal
#' }
#'
#' @examples
#' \dontrun{
#' otargen::studyInfo(studyid = "GCST90002357")
#'}
#' @importFrom magrittr %>%
#' @export
#'
#'

studyInfo <- function(studyid) {
  ## Set up to query Open Targets Genetics API
  variables <- list(studyId = studyid)

  cli::cli_progress_step("Connecting the database...", spinner = TRUE)

  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()


  query <- "query studyInfoQuery($studyId: String!){
  studyInfo(studyId:$studyId){
    studyId
    traitReported
    source
    traitEfos
    pmid
    pubDate
    pubJournal
    pubTitle
    pubAuthor
    hasSumstats
    ancestryInitial
    ancestryReplication
    nInitial
    nReplication
    nCases
    traitCategory
    numAssocLoci
    nTotal
  }
  }"


  ## Execute the query
  output_tb <- data.frame()
  otg_qry$query(name = "studyInfoQuery", x = query)

  cli::cli_progress_step("Downloading data...", spinner = TRUE)

  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$studyInfoQuery, variables),
                                   simplifyDataFrame = TRUE, flatten = TRUE)$data
  output <- result$studyInfo

  output[output == "NULL"] <- NA # replacing NULL elements with NA

  if (length(output) != 0){
  output_tb <- tibble::as_tibble(stack(unlist(output)) %>%
            tidyr::spread(ind, values)) # converting list of information key/value pairs to tibble format
  }
  return(output_tb)
}
