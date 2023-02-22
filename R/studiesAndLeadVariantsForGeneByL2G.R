#' Retrieves L2G model summary data for a gene.
#'
#' For one or more input ENSEMBL ids, a table is generated with following columns-
#' yProbaModel (L2G score), yProbaDistance, yProbaInteraction (chromatin interaction),
#' yProbaMolecularQTL, yProbaPathogenicity, pval, beta.direction, beta.betaCI, beta.betaCILower
#' beta.betaCIUpper, odds.oddsCI, odds.oddsCILower, odds.oddsCIUpper, study.studyId,
#' study.traitReported, study.traitCategory, study.pubDate, study.pubTitle, study.pubAuthor,
#' study.pubJournal, study.pmid, study.hasSumstats, study.nCases, study.numAssocLoci, study.nTotal
#' study.traitEfos, variant.id, variant.rsId, variant.chromosome, variant.position, variant.refAllele
#' variant.altAllele, variant.nearestCodingGeneDistance, variant.nearestGeneDistance, variant.mostSevereConsequence,
#' variant.nearestGene.id, variant.nearestCodingGene.id, ensembl_id, and gene_symbol.
#'
#'
#' @param ensembl_ids String: one or more gene ENSEMBL id.
#' @param l2g Float: locus to gene cut off score.
#' @param pvalue Float: pvalue cut off.
#' @param vtype Character vector: most severe consequence to filter the variants type including c(intergenic_variant",
#' "upstream_gene_variant", "intron_variant", "missense_variant", "5_prime_UTR_variant",
#' "non_coding_transcript_exon_variant", "splice_region_variant")
#'
#' @return Data frame containing the queried gene identity and its data for L2G model
#'

#' @examples
#' studiesAndLeadVariantsForGeneByL2G(ensembl_ids=list("ENSG00000163946","ENSG00000169174", "ENSG00000143001"),l2g=0.7)
#' or
#' studiesAndLeadVariantsForGeneByL2G(ensembl_ids="ENSG00000169174", l2g=0.6, pvalue=1e-8, vtype=c("intergenic_variant","intron_variant"))
#'
#' @export
#'
#'


studiesAndLeadVariantsForGeneByL2G <- function(ensembl_ids,
                                               l2g = 0.4,
                                               pvalue = 5e-8,
                                               vtype = NULL ) {


  # Check ensembl id format
  if (length(ensembl_ids) == 1){
  if (!grepl(pattern = "ENSG\\d{11}", ensembl_ids)) {
    stop("\n Please provide Ensemble gene ID")
  }
  }
  else{
    for (i in ensembl_ids){
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

  for (input_gene in ensembl_ids) {

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
      dplyr::mutate(across((yProbaModel:yProbaPathogenicity), ~ round(., 3)))

    if (!is.null(vtype)) {
      final_output <- final_output %>% dplyr::filter(variant.mostSevereConsequence %in% vtype)
    }
    }
  }

  if (nrow(final_output)!=0){
    final_output$study.traitEfos <- as.character(final_output$study.traitEfos)
  }

  return(final_output)
}
