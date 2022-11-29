#' Get all studies and lead variants for a gene
#'
#' @param ensmbl_ids is a identification id for genes by ensembl database.
#' @return A dataframe including the queried gene identity and its colocalization data
#' @examples
#' studiesAndLeadVariantsForGene(list("ENSG00000163946","ENSG00000169174", "ENSG00000143001"))
#' studiesAndLeadVariantsForGene("ENSG00000169174")
#' @export
#'
#'


studiesAndLeadVariantsForGene <- function(ensmbl_ids) {
  res2 <- data.frame()
  res_gene_info <- data.frame()

  cli::cli_progress_step("Connecting the database...", spinner = TRUE)
  con <- ghql::GraphqlClient$new("https://api.genetics.opentargets.org/graphql")
  qry <- ghql::Query$new()

  for (input_gene in ensmbl_ids) {

    cli::cli_progress_step(paste0("Downloading data for ", input_gene," ..."), spinner = TRUE)


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

    ## execute the query
    qry$query(name = "getgeninfo", x = query )

    res <- con$exec(qry$queries$getgeninfo, variables)

    res1 <- jsonlite::fromJSON(res, flatten = TRUE)
    if (!is.null(res1$data$studiesAndLeadVariantsForGene)){

    res1$data$studiesAndLeadVariantsForGene$gene_symbol <- rep(res1$data$geneInfo$symbol,
                                                               length(res1$data$studiesAndLeadVariantsForGene$study.pmid))
    res2 <- dplyr::bind_rows(res2, res1$data$studiesAndLeadVariantsForGene)
    }
    #res_gene_info <- rbind(res_gene_info, as.data.frame(res1$data$geneInfo))
    #Sys.sleep(1)
  }

  return(res2)
}
