#' Retrieves Phenome Wide Association Studies for a variant.
#'
#' PheWAS (Phenome-wide association study) is a method that investigates the relationships
#' between genetic variants and traits or phenotypes, helping in the study of their
#' potential influence on multiple traits or diseases concurrently.
#'
#' @param \emph{variantid} String: Open Targets Genetics generated ID for each variant.
#'
#' @return Returns a data frame with PheWAS associations consisting of the following columns:
#' \itemize{
#'   \item{\code{totalGWASStudies}:} \emph{Integer}. Total number of GWAS studies.
#'   \item{\code{pval}:} \emph{Numeric}. P-value.
#'   \item{\code{beta}:} \emph{Numeric}. Beta value.
#'   \item{\code{oddsRatio}:} \emph{Numeric}. Odds ratio.
#'   \item{\code{nTotal}:} \emph{Integer}. Total number of participants.
#'   \item{\code{study.studyId}:} \emph{Character vector}. Study ID.
#'   \item{\code{study.source}:} \emph{Character vector}. Source of the study.
#'   \item{\code{study.pmid}:} \emph{Character vector}. PMID (PubMed ID) of the study.
#'   \item{\code{study.pubDate}:} \emph{Character vector}. Publication date of the study.
#'   \item{\code{study.traitReported}:} \emph{Character vector}. Reported trait.
#'   \item{\code{study.traitCategory}:} \emph{Character vector}. Trait category.
#' }
#'
#' @examples
#' \dontrun{
#' result <- pheWAS(variantid = "1_154549918_C_A")
#' result <- pheWAS(variantid = "rs72698179")
#' }
#' @import dplyr
#' @importFrom magrittr %>%
#' @export
#'

pheWAS <- function(variantid) {

  # make connection to the endpoint

  cli::cli_progress_step("Connecting the database...", spinner = TRUE)
  client <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  query_class <- ghql::Query$new()

  # Check variant id format
  if (grepl(pattern = "rs\\d+", variantid)) {

    # Convert rs id to variant id
    query_searchid <- "query ConvertRSIDtoVID($queryString:String!) {
    search(queryString:$queryString){
      totalVariants
      variants{
        id
        }
      }
    }"

    variables <- list(queryString = variantid)
    query_class$query(name = "convertid", x = query_searchid)
    id_result <- jsonlite::fromJSON(client$exec(query_class$queries$convertid, variables), flatten=TRUE)$data
    input_variantid <- id_result$search$variants$id
  }

  else if (grepl(pattern = "\\d+_\\d+_[a-zA-Z]+_[a-zA-Z]+", variantid))
  {
    input_variantid <- variantid
  }
  else
  {
    stop("\n Please provide a variant Id")
  }


  ## Set up to query Open Targets Genetics API

  query <- 'query search($variantId: String!){
    pheWAS(variantId: $variantId){
    totalGWASStudies
    associations{
      pval
      beta
      oddsRatio
      study{
        studyId
        source
        pmid
        pubDate
       traitReported
        traitCategory
      }
      nTotal
    }
  }
}'


  # execute the query

  variables <- list(variantId = input_variantid)

  query_class$query(name ='phewas_query' , x = query )

  cli::cli_progress_step(paste("Downloading data...",variantid,"..."), spinner = TRUE)

  result <- jsonlite::fromJSON(client$exec(query_class$queries$phewas_query, variables), flatten=TRUE)$data
  result_df <- data.frame()
  if (length(result$pheWAS$associations)!=0){
    result_df <- result$pheWAS %>% as.data.frame
  }

  base::colnames(result_df) <- stringr::str_replace_all(colnames(result_df),"associations.","")
  return (result_df)
}
