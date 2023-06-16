#' Retrieves GWAS study id information.
#'
#' For a given study id, this function returns all the relevant information about
#' a GWAS study, such as PubMed ID, studied trait EFO ID, case/control size, etc.
#'
#' @param \emph{studyid} String: Open Targets Genetics generated ID for a GWAS study.
#'
#' @return Returns a tibble data table containing the GWAS study information. The table consists of the following columns:
#' \itemize{
#'   \item{\code{studyId}:} \emph{Character}. Study ID.
#'   \item{\code{traitReported}:} \emph{Character}. Reported trait.
#'   \item{\code{source}:} \emph{Character}. Source.
#'   \item{\code{traitEfos}:} \emph{Character}. Trait EFO ID.
#'   \item{\code{pmid}:} \emph{Character}. PubMed ID.
#'   \item{\code{pubDate}:} \emph{Character}. Publication date.
#'   \item{\code{pubJournal}:} \emph{Character}. Publication journal.
#'   \item{\code{pubTitle}:} \emph{Character}. Publication title.
#'   \item{\code{pubAuthor}:} \emph{Character}. Publication author.
#'   \item{\code{hasSumstats}:} \emph{Character}. Indicates if the study has summary statistics.
#'   \item{\code{ancestryInitial}:} \emph{Character}. Initial ancestry.
#'   \item{\code{nInitial}:} \emph{Character}. Initial sample size.
#'   \item{\code{nReplication}:} \emph{Character}. Replication sample size.
#'   \item{\code{traitCategory}:} \emph{Character}. Trait category.
#'   \item{\code{numAssocLoci}:} \emph{Character}. Number of associated loci.
#'   \item{\code{nTotal}:} \emph{Character}. Total sample size.
#' }
#'
#' @examples
#' \dontrun{
#' result <- studyInfo(studyid = "GCST90002357")
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
