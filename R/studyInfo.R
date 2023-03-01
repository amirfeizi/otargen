#' Retrieves GWAS study id information.
#'
#' For a given study id, this function returns all the  relevant information about
#'  GWAS study such as PubMed id, studied trait EFO id, case/control size etc.
#'  See the example for full list of columns names
#' columns mentioned below.
#'
#' @param studyid String: Open Targets Genetics generated id for a GWAS study.
#'
#' @return tibble data table containing the study information.
#'
#' @examples
#' study_info <- studyInfo(studyid = "GCST90002357")
#' study_info
#'
#' # A tibble: 1 × 17
#' # studyId      traitReported  source traitEfos   pmid         pubDate pubJo…¹ pubTi…² pubAu…³ hasSu…⁴ ances…⁵ nInit…⁶ nRepl…⁷ nCases trait…⁸ numAs…⁹ nTotal
#' #<chr>        <chr>          <chr>  <chr>       <chr>        <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  <chr>   <chr>   <chr>
#' # GCST90002357 Platelet count GCST   EFO_0004309 PMID:328884… 2020-0… Cell    Trans-… Chen MH TRUE    Europe… 542827  NA      NA     measur… 1252    542827
#' # with abbreviated variable names ¹pubJournal, ²pubTitle, ³pubAuthor, ⁴hasSumstats, ⁵ancestryInitial, ⁶nInitial, ⁷nReplication, ⁸traitCategory, ⁹numAssocLoci
#'
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

  otg_qry$query(name = "studyInfoQuery", x = query)

  cli::cli_progress_step("Downloading data...", spinner = TRUE)

  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$studyInfoQuery, variables),
                                   simplifyDataFrame = TRUE, flatten = TRUE)$data
  output <- result$studyInfo

  output[output == "NULL"] <- NA # replacing NULL elements with NA

  output_tb <- tibble::as_tibble(stack(unlist(output)) %>%
            tidyr::spread(ind, values)) # converting list of information key/value pairs to tibble format
  return(output_tb)
}
