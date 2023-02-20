#' Query L2G model summary data for a gene
#'
#' @param ensmbl_ids is a identification id for genes by ensembl database.
#' @param l2g is locus to gene cut off, the defaul is set to 0.4.
#' @param vtype is a vector of variants most severe consequence to filter the variants type including c("all","intergenic_variant",
#' "upstream_gene_variant", "intron_variant", "missense_variant", "5_prime_UTR_variant","non_coding_transcript_exon_variant", "splice_region_variant"). The default
#' is "all".
#' @return A dataframe including the queried gene indentity and its colocalization data for L2G model
#' @examples
#' studiesAndLeadVariantsForGeneByL2G(list("ENSG00000163946","ENSG00000169174", "ENSG00000143001"))
#' studiesAndLeadVariantsForGeneByL2G("ENSG00000169174")
#' studiesAndLeadVariantsForGeneByL2G("ENSG00000169174", l2g=0.6)
#' @export
#'
#'

studiesAndLeadVariantsForGeneByL2G <- function(ensmbl_ids, l2g = 0.4, pvalue = 1e-8, vtype = c("all") ) {

  # Check ensembl id format
  if (length(ensmbl_ids) == 1){
  if (!grepl(pattern = "ENSG\\d{11}", ensmbl_ids)) {
    stop("\n Please provide Ensemble gene ID")
  }
  }
  else{
    for (i in ensmbl_ids){
      if (!grepl(pattern = "ENSG\\d{11}", i)) {
        stop("\n Please provide Ensemble gene ID")
      }
    }

  }

  query <- "query	studiesAndLeadl2g($gene:String!) {

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
                               odds {
                                   oddsCI
                                   oddsCILower
                                   oddsCIUpper

                                   }
                               study {
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
                               variant {
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

 }"

  final_output <- data.frame()

  # make a graphql connection

  cli::cli_progress_step("Connecting the database...", spinner = TRUE)
  con <- ghql::GraphqlClient$new("https://api.genetics.opentargets.org/graphql")

  for (input_gene in ensmbl_ids) {

    variables <- list(gene = input_gene)
    cli::cli_progress_step(paste0("Downloading data for ", input_gene," ..."), spinner = TRUE)

    # Set up to query Open Targets Platform API
    qry <- ghql::Query$new()

    # Query for targets associated with a disease and L2G scores

    qry$query(name = "getStudiesLeadL2G", x = query)

    ## Execute the query
    output0 <- con$exec(qry$queries$getStudiesLeadL2G, variables) # execute the query

    output1 <- jsonlite::fromJSON(output0, flatten = TRUE) # convert the query output from json

    if (length(output1$data$studiesAndLeadVariantsForGeneByL2G) != 0){

    ## tidying the output

    output1$data$studiesAndLeadVariantsForGeneByL2G$ensembl_id <- rep(
      output1$data$geneInfo$id,
      length(output1$data$studiesAndLeadVariantsForGeneByL2G$yProbaModel)
    )

    output1$data$studiesAndLeadVariantsForGeneByL2G$gene_symbol <- rep(
      output1$data$geneInfo$symbol,
      length(output1$data$studiesAndLeadVariantsForGeneByL2G$yProbaModel)
    )

    final_output <- dplyr::bind_rows(final_output, output1$data$studiesAndLeadVariantsForGeneByL2G) %>%
      dplyr::filter(yProbaModel >= l2g, pval <= pvalue) %>%
      dplyr::mutate(across(where(is.numeric), ~ round(., 2)))

    if (vtype !="all") {

      final_output1 <- final_output %>% dplyr::filter(variant.mostSevereConsequence %in% vtype)
      return(final_output1)
    }

    # Sys.sleep(1)
    }
  }

  return(final_output)
}
