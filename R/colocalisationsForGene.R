#' Get colocalization data for a gene
#'
#' @param ensmbl_ids is a identification id for genes by ensembl database
#' @return a dataframe including the queried gene indentity and its colocalization data
#' @export

colocalisationsForGene <- function(ensmbl_ids) {
  ensmbl_ids <- ensmbl_ids
  colocal2 <- data.frame()
  colocal_genes_info <- data.frame()



  for (input_gene in ensmbl_ids) {
    print(input_gene)
    qry <- ghql::Query$new()
    qry$query("getgenColocal", "query	geneandcolocal($gene:String!) {
  geneInfo (geneId:$gene) {
    id
    symbol
    description
    chromosome
    start
    end

  }


colocalisationsForGene(geneId:$gene){
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
    id
  }
  phenotypeId
  log2h4h3
  qtlStudyId
  h3
  h4
}

}")

    variables <- list(gene = input_gene)
    con <- ghql::GraphqlClient$new("https://api.genetics.opentargets.org/graphql")

    colocal <- con$exec(qry$queries$getgenColocal, variables)
    colocal1 <- jsonlite::fromJSON(colocal, flatten = TRUE)

    colocal_genes_info <- rbind(colocal_genes_info, as.data.frame(colocal1$data$geneInfo))

    colocal1$data$colocalisationsForGene$gene_symbol <- rep(
      colocal1$data$geneInfo$symbol,
      length(colocal1$data$colocalisationsForGene$phenotypeId)
    )
    colocal2 <- rbind(colocal2, colocal1$data$colocalisationsForGene)


    # Sys.sleep(1)
  }
  return(colocal2)
}
