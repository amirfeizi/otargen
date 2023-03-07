#' Retrieves L2G model summary data for a gene.
#'
#' For one or more input ENSEMBL ids or gene names, a table is generated with following columns-
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
#' @param genes String: one or more gene ENSEMBL id or gene names.
#' @param l2g Float: locus to gene cut off score.
#' @param pvalue Float: pvalue cut off.
#' @param vtype Character vector: most severe consequence to filter the variants type including c(intergenic_variant",
#' "upstream_gene_variant", "intron_variant", "missense_variant", "5_prime_UTR_variant",
#' "non_coding_transcript_exon_variant", "splice_region_variant")
#'
#' @return Data frame containing the queried gene identity and its data for L2G model
#'

#' @examples
#' \dontrun{
#' otargen::studiesAndLeadVariantsForGeneByL2G(genes =
#' list("ENSG00000163946",
#'  "ENSG00000169174", "ENSG00000143001"), l2g = 0.7)
#' otargen::studiesAndLeadVariantsForGeneByL2G(
#' genes = "ENSG00000169174",
#' l2g = 0.6, pvalue = 1e-8,
#' vtype = c("intergenic_variant", "intron_variant"))
#' otargen::studiesAndLeadVariantsForGeneByL2G(genes ="TMEM61")
#'}
#' @importFrom magrittr %>%
#' @export
#'
#'


studiesAndLeadVariantsForGeneByL2G <- function(genes,
                                               l2g = 0.4,
                                               pvalue = 5e-8,
                                               vtype = NULL) {

  # make a graphql connection

  cli::cli_progress_step("Connecting the database...", spinner = TRUE)
  con <- ghql::GraphqlClient$new("https://api.genetics.opentargets.org/graphql")
  # Set up to query Open Targets Platform API
  qry <- ghql::Query$new()

  # Check gene format
  #Query for gene name search
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

  for (input_gene in ensembl_ids) {
    variables <- list(gene = input_gene)
    cli::cli_progress_step(paste0("Downloading data for ", input_gene, " ..."), spinner = TRUE)

    # Query for targets associated with a disease and L2G scores

    qry$query(name = "getStudiesLeadL2G", x = query)

    ## Execute the query
    output0 <- con$exec(qry$queries$getStudiesLeadL2G, variables) # execute the query

    output1 <- jsonlite::fromJSON(output0, flatten = TRUE) # convert the query output from json


    if (length(output1$data$studiesAndLeadVariantsForGeneByL2G) != 0) {
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

  if (nrow(final_output) != 0) {
    final_output$study.traitEfos <- as.character(final_output$study.traitEfos)
  }

  return(final_output)
}
