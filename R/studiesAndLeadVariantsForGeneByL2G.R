#' Query L2G model summary data for a gene
#'
#' @param ensmbl_ids is a identification id for genes by ensembl database
#' @return a dataframe including the queried gene indentity and its colocalization data
#' @export

studiesAndLeadVariantsForGeneByL2G <- function(ensmbl_ids) {

  # Check ensembl id format

  if (!grepl(pattern ="ENSG\\d{11}", ensmbl_ids)) {
    stop("\n Please provide Ensemble gene ID")
  }

  ensmbl_ids <- ensmbl_ids
  l2g2 <- data.frame()
  l2g_genes_info <- data.frame()
  con <- ghql::GraphqlClient$new("https://api.genetics.opentargets.org/graphql") # make a graphql connection

  for (input_gene in ensmbl_ids) {
    base::print(input_gene)

    # Set up to query Open Targets Platform API
    qry <- ghql::Query$new()

    # Query for targets associated with a disease and L2G scores

    qry$query("getStudiesLeadL2G", "query	studiesAndLeadl2g($gene:String!) {

             geneInfo (geneId:$gene) {
                                     id
                                     symbol
                                     description
                                     chromosome
                                     start
                                     end
                                     }
             studiesAndLeadVariantsForGeneByL2G (geneId:$gene){
                                     yProbaModel
                                     yProbaDistance
                                     yProbaInteraction
                                     yProbaMolecularQTL
                                     yProbaPathogenicity
                                     pval
                               beta {

                                     direction
                                     betaCI
                                     betaCILower
                                     betaCIUpper
                                   }
                               odds{
                                   oddsCI
                                   oddsCILower
                                   oddsCIUpper

                                   }
                               study{
                                 studyId
                                 traitReported
                                 traitCategory
                                 pubDate
                                 pubTitle
                                 pubAuthor
                                 pubJournal
                                 pmid
                                 hasSumstats
                                 nCases
                                 numAssocLoci
                                 nTotal
                                 traitEfos

                                     }
                               variant{
                                 id
                                 rsId
                                 chromosome
                                 position
                                 refAllele
                                 altAllele
                               nearestGene {
                                 id
                                           }
                                nearestCodingGene {
                                 id
                                 }
                               nearestCodingGeneDistance
                               nearestGeneDistance
                               mostSevereConsequence

     }
   }

 }")

    ## Execute the query

    variables <- list(gene = input_gene) # define the input gene name

    l2g <- con$exec(qry$queries$getStudiesLeadL2G, variables) # execute the query

    l2g1 <- jsonlite::fromJSON(l2g, flatten = TRUE) # convert the query output from json


    l2g1$data$studiesAndLeadVariantsForGeneByL2G$gene_symbol <- rep(l2g1$data$geneInfo$symbol,
                                                                    length(l2g1$data$studiesAndLeadVariantsForGeneByL2G$yProbaModel))

    l2g2 <- dplyr::bind_rows(l2g2, l2g1$data$studiesAndLeadVariantsForGeneByL2G)
    gene_info <- l2g1$data$geneInfo
    l2g_genes_info <- dplyr::bind_rows(l2g_genes_info, gene_info)

    # Sys.sleep(1)
  }

  return(l2g2)
}
