#' Retrieve PheWAS (Phenome Wide Association Studies) data for a variant.
#'
#' PheWAS (\emph{Phenome-wide association study}) is a method that investigates the relationships
#' between genetic variants and traits or phenotypes, helping in the study of their
#' potential influence on multiple traits or diseases concurrently. This function retrieves
#' the traits associated with a given variant in the \emph{UK Biobank}, \emph{FinnGen},
#' and/or \emph{GWAS Catalog} summary statistics repository (only traits with a p-value
#' less than 0.005 are returned).
#'
#' @param variant_id Character: generated ID for variants by Open Targets Genetics (e.g. 1_154119580_C_A) or rsId (rs2494663).
#'
#' @return A data frame with PheWAS associations.
#'
#' The output data frame contains the following columns:
#' \itemize{
#'   \item{\code{totalGWASStudies}:} An integer indicating the total number of GWAS studies where the variant is associated.
#'   \item{\code{pval}:} A numeric value representing the p-value of the association between the variant and the trait.
#'   \item{\code{beta}:} A numeric value representing the beta value, which represents the effect size of the variant on the trait.
#'   \item{\code{oddsRatio}:} A numeric value representing the odds ratio, measuring the association between the variant and the trait.
#'   \item{\code{nTotal}:} An integer indicating the total number of participants in the study.
#'   \item{\code{study.studyId}:} A character vector representing the study ID.
#'   \item{\code{study.source}:} A character vector representing the source of the study.
#'   \item{\code{study.pmid}:} A character vector representing the PubMed ID (PMID) of the study.
#'   \item{\code{study.pubDate}:} A character vector representing the publication date of the study.
#'   \item{\code{study.traitReported}:} A character vector representing the reported trait associated with the variant.
#'   \item{\code{study.traitCategory}:} A character vector representing the category of the trait.
#' }
#'
#' @examples
#' \dontrun{
#' result <- pheWAS(variant_id = "1_154549918_C_A")
#' result <- pheWAS(variant_id = "rs72698179")
#' }
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom ghql GraphqlClient
#' @importFrom ghql Query
#' @importFrom jsonlite fromJSON
#' @export
#'
#' @references
#' Pendergrass, S A et al. “The use of phenome-wide association studies (PheWAS) for exploration of
#' novel genotype-phenotype relationships and pleiotropy discovery.” Genetic epidemiology vol. 35,5 (2011): 410-22. doi:10.1002/gepi.20589
#'
#'

pheWAS <- function(variant_id) {
  # make connection to the endpoint

tryCatch({
  cli::cli_progress_step("Connecting to the Open Targets Genetics GrpahQL API...", spinner = TRUE)
  client <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  query_class <- ghql::Query$new()

  # Check variant id format
  if (grepl(pattern = "rs\\d+", variant_id)) {
    # Convert rs id to variant id
    query_searchid <- "query ConvertRSIDtoVID($queryString:String!) {
      search(queryString:$queryString){
        totalVariants
        variants{
          id
        }
      }
    }"

    variables <- list(queryString = variant_id)
    query_class$query(name = "convertid", x = query_searchid)
    id_result <- jsonlite::fromJSON(client$exec(query_class$queries$convertid, variables), flatten = TRUE)$data
    input_variant_id <- id_result$search$variants$id
  } else if (grepl(pattern = "\\d+_\\d+_[a-zA-Z]+_[a-zA-Z]+", variant_id)) {
    input_variant_id <- variant_id
  } else {
    stop("\n Please provide a variant ID")
  }

  ## Set up to query Open Targets Genetics API
  query <- 'query search($variantId: String!) {
    pheWAS(variantId: $variantId) {
      totalGWASStudies
      associations {
        pval
        beta
        oddsRatio
        study {
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
  variables <- list(variantId = input_variant_id)
  query_class$query(name = 'phewas_query', x = query)
  cli::cli_progress_step(paste("Downloading data for variant", variant_id, "..."), spinner = TRUE)
  result <- jsonlite::fromJSON(client$exec(query_class$queries$phewas_query, variables), flatten = TRUE)$data
  result_df <- data.frame()
  if (length(result$pheWAS$associations) != 0) {
    result_df <- result$pheWAS %>% as.data.frame
  }
  base::colnames(result_df) <- stringr::str_replace_all(colnames(result_df), "associations.", "")
  return(result_df)

}, error = function(e) {
  # Handling connection timeout
  if(grepl("Timeout was reached", e$message)) {
    stop("Connection timeout reached while connecting to the Open Targets Genetics GraphQL API.")
  } else {
    stop(e) # Handle other types of errors
  }
})
}
