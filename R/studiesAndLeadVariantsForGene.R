#' Retrieves all studies and lead variants for a gene
#'
#' @param genes String: one or more gene ENSEMBL id or gene name.
#'
#' @return Returns a dataframe
#'
#' @examples
#' \dontrun{
#' otargen::studiesAndLeadVariantsForGene(genes = list("ENSG00000163946","ENSG00000169174", "ENSG00000143001"))
#' otargen::studiesAndLeadVariantsForGene(genes = "ENSG00000169174")
#' otargen::studiesAndLeadVariantsForGene(genes = list("PCSK9","TASOR", "TMEM61"))
#' otargen::studiesAndLeadVariantsForGene(genes = "PCSK9")
#'}
#'
#' @importFrom magrittr %>%
#' @export
#'
#'


studiesAndLeadVariantsForGene <- function(genes) {
  cli::cli_progress_step("Connecting the database...", spinner = TRUE)
  con <- ghql::GraphqlClient$new("https://api.genetics.opentargets.org/graphql")
  qry <- ghql::Query$new()

  #Query for gene name search
  # check for gene name:
  query_search <- "query convertnametoid($queryString:String!) {
    search(queryString:$queryString){
      genes{
        id
        symbol
      }
      }
    }"

  # Check format
  match_result <- grepl(pattern = "ENSG\\d{11}", genes)
  df_id = data.frame()

  if (all(match_result) == FALSE){
    for (g in genes) {
      variables <- list(queryString = g)
      qry$query(name = "convertnametoid", x = query_search)
      id_result <- jsonlite::fromJSON(con$exec(qry$queries$convertnametoid, variables), flatten = TRUE)$data
      id <- as.data.frame(id_result$search$genes)
      if (nrow(id)!=0){
        name_match <- id[id$symbol == g, ]
        ensembl_ids <- name_match$id
        df_id <- dplyr::bind_rows(df_id, as.data.frame(ensembl_ids))
      }
    }
    if (nrow(df_id)==0){
      stop("\nPlease provide Ensemble gene ID or gene name")
    } else {
      ensembl_ids <- as.list(df_id$ensembl_ids)
    }
  } else {
    ensembl_ids <- genes
  }

  for (input_gene in ensembl_ids) {
    cli::cli_progress_step(paste0("Downloading data for ", input_gene, " ..."), spinner = TRUE)
    variables <- list(gene = input_gene)


    query <- "query	geneandstudy($gene:String!) {
   geneInfo (geneId:$gene) {
     id
     symbol
     description
     chromosome
     start
     end

   }
   studiesAndLeadVariantsForGene(geneId:$gene){
     study {
       pmid
       pubDate
       pubJournal
       pubTitle
       pubAuthor
       hasSumstats
       nInitial
       nReplication
       nCases
       traitCategory
       numAssocLoci
     }

   }
 }"

    final_out <- data.frame()

    ## execute the query
    qry$query(name = "getgeninfo", x = query)

    study_var_result <- con$exec(qry$queries$getgeninfo, variables)

    study_var_result1 <- jsonlite::fromJSON(study_var_result, flatten = TRUE)

    if (!is.null(study_var_result1$data$studiesAndLeadVariantsForGene)) {
      study_var_result1$data$studiesAndLeadVariantsForGene$gene_symbol <- rep(
        study_var_result1$data$geneInfo$symbol,
        length(study_var_result1$data$studiesAndLeadVariantsForGene$study.pmid)
      )

      final_out <- dplyr::bind_rows(final_out, study_var_result1$data$studiesAndLeadVariantsForGene)
    }
  }

  return(final_out)
}
