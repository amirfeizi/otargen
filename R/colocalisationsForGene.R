#' Get colocalization data for a gene
#'
#' @param ensmbl_ids is a identification id for genes by ensembl database
#' @return A dataframe including the queried gene indentity and its colocalization data
#' @examples
#' colocalisationsForGene(list("ENSG00000163946","ENSG00000169174", "ENSG00000143001"))
#' colocalisationsForGene("ENSG00000169174")
#' @export

colocalisationsForGene <- function(ensmbl_ids) {



  # Check ensembl id format

  if (!grepl(pattern = "ENSG\\d{11}", ensmbl_ids)) {
    stop("\n Please provide Ensemble gene ID")
  }

  ensmbl_ids <- ensmbl_ids

  colocal2 <- data.frame()
  colocal_genes_info <- data.frame()

  query <- "query	geneandcolocal($gene:String!) {
  geneInfo (geneId:$gene) {
    id
    symbol
    description
    chromosome
    start
    end

  }

colocalisationsForGene(geneId:$gene){
  leftVariant {
      id
      rsId
    }
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
  tissue {
    name
    __typename
  }
  phenotypeId
  h3
  h4
  log2h4h3
  qtlStudyId
  __typename

}

}"
  cli_progress_step("Connecting the database...")
  con <- ghql::GraphqlClient$new("https://api.genetics.opentargets.org/graphql")
  qry <- ghql::Query$new()

  for (input_gene in ensmbl_ids) {

    print(input_gene)
    cli_progress_step(paste0("Downloading data for ", input_gene," ..."))


    variables <- list(gene = input_gene)

    qry$query(name = "getgenColocal", x = query)

    colocal <- con$exec(qry$queries$getgenColocal, variables)
    colocal1 <- jsonlite::fromJSON(colocal, flatten = TRUE)

    colocal_genes_info <- rbind(colocal_genes_info, as.data.frame(colocal1$data$geneInfo))

    colocal1$data$colocalisationsForGene$gene_symbol <- rep(
      colocal1$data$geneInfo$symbol,
      length(colocal1$data$colocalisationsForGene$phenotypeId)
    )
    colocal2 <- rbind(colocal2, colocal1$data$colocalisationsForGene)
    cli_progress_update()
   }
  return(colocal2)
}
