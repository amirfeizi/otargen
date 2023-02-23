#' Retrieves all studies and lead variants for a gene
#'
#' @param ensembl_ids String: one or more gene ENSEMBL id.
#'
#' @return Dataframe with the queried gene identity and its colocalization data
#'
#' @examples
#' studiesAndLeadVariantsForGene(ensembl_ids = list("ENSG00000163946", "ENSG00000169174", "ENSG00000143001"))
#' or
#' studiesAndLeadVariantsForGene(ensembl_ids = "ENSG00000169174")
#'
#' @export
#'
#'


studiesAndLeadVariantsForGene <- function(ensembl_ids) {
  cli::cli_progress_step("Connecting the database...", spinner = TRUE)
  con <- ghql::GraphqlClient$new("https://api.genetics.opentargets.org/graphql")
  qry <- ghql::Query$new()

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
