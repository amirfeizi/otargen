#' Retrieves Phenome Wide Association Studies
#'
#' For an input variant id, a table is generated with the following columns-
#' totalGWASStudies, studyId, pval, beta, eaf(effect allele frequency), se(standard error),
#' oddsRatio, nTotal(total sample size), nCases, study.source, study.pmid, study,pudDate,
#' study.traiReported, and study.traitCategory.
#'
#'
#' @param variantid String: Open Target Genetics generated index variant id or rs id.
#' @param pageIndex Int: Index of the current page, pagination index >= 0.
#' @param pageSize Int: No. of records in a page, pagination size > 0.
#'
#' @returns A data frame with PheWAS associations in it
#'
#' @examples
#' pheWAS(variantid = "1_154549918_C_A")
#' or
#' pheWAS(variantid = "1_154562290_G_A", pageindex = 1, pagesize = 50)
#'
#' @export
#'
#'

pheWAS <- function(variantid, pageindex = 0, pagesize = 20) {
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
    id_result <- jsonlite::fromJSON(client$exec(query_class$queries$convertid, variables), flatten = TRUE)$data
    input_variantid <- id_result$search$variants$id
  } else if (grepl(pattern = "\\d+_\\d+_[a-zA-Z]+_[a-zA-Z]+", variantid)) {
    input_variantid <- variantid
  } else {
    stop("\n Please provide a variant Id")
  }


  ## Set up to query Open Targets Genetics API

  query <- "query search($variantId: String!){
    pheWAS(variantId: $variantId){
    totalGWASStudies
    associations{
      studyId
      pval
      beta
      eaf
      se
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
      nCases
    }
  }
}"


  # execute the query

  variables <- list(variantId = input_variantid, pageIndex = pageindex, pageSize = pagesize)

  query_class$query(name = "phewas_query", x = query)

  cli::cli_progress_step(paste("Downloading data...", variantid, "..."), spinner = TRUE)

  phewas_out <- jsonlite::fromJSON(client$exec(query_class$queries$phewas_query, variables), flatten = TRUE)$data
  df_phewas <- data.frame()
  if (length(phewas_out$pheWAS$associations) != 0) {
    df_phewas <- phewas_out$pheWAS %>% as.data.frame()
    base::colnames(df_phewas) <- stringr::str_replace_all(colnames(df_phewas), "associations.", "")
  }

  return(df_phewas)
}
