#' Gets the information about the input study id which links top loci with a trait.
#'
#'
#' @param studyid is the Open Target Genetics generated id for gwas studies.
#' @return A dataframe containing the study information.
#' @examples
#' studyInfo("GCST90002357")
#' @export
#'

studyInfo <- function(studyid) {

  ## Set up to query Open Targets Genetics API
  variables <- studyid

  cli::cli_progress_step("Connecting the database...", spinner = TRUE)
  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()


  query <- "query studyInfoquery($studyId: String!){
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

  otg_qry$query(name = "studyInfoquery", x =  query)

  cli::cli_progress_step("Downloading data...", spinner = TRUE)
  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$studyInfoquery, variables), simplifyDataFrame = TRUE, flatten = TRUE)$data
<<<<<<< HEAD
  output <- tibble::as_tibble(result[[1]])
  print (result)
=======
  result <- result$studyInfo
  result[result == "NULL"] <- NA # replacing NULL elements with NA
  output <- tibble::as.tibble(stack(unlist(result)) %>% tidyr::spread(ind,values)) # converting list of information key/value pairs to tibble format

>>>>>>> b6f57bfcf9416ee5a3ffd1ac35aed8f9b9616570
}
