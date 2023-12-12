#' Retrieve "locus-to-gene" (L2G) model summary data for a gene.
#'
#' The "locus-to-gene" (L2G) model derives features to prioritize likely causal genes at each GWAS
#' locus based on genetic and functional genomics features. The main categories of predictive features are:
#' \itemize{
#' \item Distance: Distance from credible set variants to the gene.
#' \item Molecular QTL colocalization: Colocalization with molecular QTLs.
#' \item Chromatin interaction: Interactions, such as promoter-capture Hi-C.
#' \item Variant pathogenicity: Pathogenicity scores from VEP (Variant Effect Predictor).
#' }
#'
#'The function also provides additional filtering parameters to narrow the results based following parameters (see below)
#'
#' @param gene Character: Gene ENSEMBL ID (e.g. ENSG00000169174) or gene symbol (e.g. PCSK9). This argument can take a list of genes too.
#' @param l2g Numeric: Locus-to-gene (L2G) cutoff score. (Default: NA)
#' @param pvalue Character: P-value cutoff. (Default: NA)
#' @param vtype Character: Most severe consequence to filter the variant types, including "intergenic_variant",
#' "upstream_gene_variant", "intron_variant", "missense_variant", "5_prime_UTR_variant",
#' "non_coding_transcript_exon_variant", "splice_region_variant". (Default: NULL)
#'
#' @return Returns a data frame containing the input gene ID and its data for the L2G model. The table consists of the following columns:
#' \itemize{
#'   \item{\code{yProbaModel}:} \emph{Numeric}. L2G score.
#'   \item{\code{yProbaDistance}:} \emph{Numeric}. Distance.
#'   \item{\code{yProbaInteraction}:} \emph{Numeric}. Chromatin interaction.
#'   \item{\code{yProbaMolecularQTL}:} \emph{Numeric}. Molecular QTL.
#'   \item{\code{yProbaPathogenicity}:} \emph{Numeric}. Pathogenicity.
#'   \item{\code{pval}:} \emph{Numeric}. P-value.
#'   \item{\code{beta.direction}:} \emph{Character}. Beta direction.
#'   \item{\code{beta.betaCI}:} \emph{Numeric}. Beta confidence interval.
#'   \item{\code{beta.betaCILower}:} \emph{Numeric}. Lower bound of the beta confidence interval.
#'   \item{\code{beta.betaCIUpper}:} \emph{Numeric}. Upper bound of the beta confidence interval.
#'   \item{\code{odds.oddsCI}:} \emph{Numeric}. Odds ratio confidence interval.
#'   \item{\code{odds.oddsCILower}:} \emph{Numeric}. Lower bound of the odds ratio confidence interval.
#'   \item{\code{odds.oddsCIUpper}:} \emph{Numeric}. Upper bound of the odds ratio confidence interval.
#'   \item{\code{study.studyId}:} \emph{Character}. Study ID.
#'   \item{\code{study.traitReported}:} \emph{Character}. Reported trait.
#'   \item{\code{study.traitCategory}:} \emph{Character}. Trait category.
#'   \item{\code{study.pubDate}:} \emph{Character}. Publication date.
#'   \item{\code{study.pubTitle}:} \emph{Character}. Publication title.
#'   \item{\code{study.pubAuthor}:} \emph{Character}. Publication author.
#'   \item{\code{study.pubJournal}:} \emph{Character}. Publication journal.
#'   \item{\code{study.pmid}:} \emph{Character}. PubMed ID.
#'   \item{\code{study.hasSumstats}:} \emph{Logical}. Indicates if the study has summary statistics.
#'   \item{\code{study.nCases}:} \emph{Integer}. Number of cases in the study.
#'   \item{\code{study.numAssocLoci}:} \emph{Integer}. Number of associated loci.
#'   \item{\code{study.nTotal}:} \emph{Integer}. Total number of samples in the study.
#'   \item{\code{study.traitEfos}:} \emph{Character}. Trait EFOs.
#'   \item{\code{variant.id}:} \emph{Character}. Variant ID.
#'   \item{\code{variant.rsId}:} \emph{Character}. Variant rsID.
#'   \item{\code{variant.chromosome}:} \emph{Character}. Variant chromosome.
#'   \item{\code{variant.position}:} \emph{Integer}. Variant position.
#'   \item{\code{variant.refAllele}:} \emph{Character}. Variant reference allele.
#'   \item{\code{variant.altAllele}:} \emph{Character}. Variant alternate allele.
#'   \item{\code{variant.nearestCodingGeneDistance}:} \emph{Integer}. Distance to the nearest coding gene.
#'   \item{\code{variant.nearestGeneDistance}:} \emph{Integer}. Distance to the nearest gene.
#'   \item{\code{variant.mostSevereConsequence}:} \emph{Character}. Most severe consequence.
#'   \item{\code{variant.nearestGene.id}:} \emph{Character}. Nearest gene ID.
#'   \item{\code{variant.nearestCodingGene.id}:} \emph{Character}. Nearest coding gene ID.
#'   \item{\code{ensembl_id}:} \emph{Character}. Ensembl ID.
#'   \item{\code{gene_symbol}:} \emph{Character}. Gene symbol.
#' }
#'
#' @examples
#' \dontrun{
#' result <- studiesAndLeadVariantsForGeneByL2G(genes = c("ENSG00000163946",
#'      "ENSG00000169174", "ENSG00000143001"), l2g = 0.7)
#' result <- studiesAndLeadVariantsForGeneByL2G(genes = "ENSG00000169174",
#'      l2g = 0.6, pvalue = 1e-8, vtype = c("intergenic_variant", "intron_variant"))
#' result <- studiesAndLeadVariantsForGeneByL2G(genes = "TMEM61")
#'}
#' @importFrom magrittr %>%
#' @export
#'
#'
studiesAndLeadVariantsForGeneByL2G <- function(gene, l2g = NA, pvalue = NA, vtype = NULL) {
  if (missing(gene) || is.null(gene)) {
    message("Please provide a value for the 'gene' argument.")
    return(NULL)
  }

  # Set up to query Open Targets Genetics API

tryCatch({
  cli::cli_progress_step("Connecting to the Open Targets Genetics GrpahQL API...", spinner = TRUE)
  con <- ghql::GraphqlClient$new("https://api.genetics.opentargets.org/graphql")
  qry <- ghql::Query$new()

  # Check gene format
  query_search <- "query convertnametoid($queryString: String!) {
    search(queryString: $queryString) {
      genes {
        id
        symbol
      }
    }
  }"

  # Check format
  match_result <- grepl(pattern = "ENSG\\d{11}", gene)
  df_id <- data.frame()

  if (all(match_result) == FALSE) {
    for (g in gene) {
      variables <- list(queryString = g)
      qry$query(name = "convertnametoid", x = query_search)
      id_result <- jsonlite::fromJSON(con$exec(qry$queries$convertnametoid, variables), flatten = TRUE)$data
      id <- as.data.frame(id_result$search$genes)
      if (nrow(id) != 0) {
        name_match <- id[id$symbol == g, ]
        ensembl_ids <- name_match$id
        df_id <- dplyr::bind_rows(df_id, as.data.frame(ensembl_ids))
      }
    }
    if (nrow(df_id) == 0) {
      stop("\nPlease provide Ensemble gene ID or gene name")
    } else {
      ensembl_ids <- as.list(df_id$ensembl_ids)
    }
  } else {
    ensembl_ids <- gene
  }

  query <- "query studiesAndLeadl2g($gene: String!) {
    geneInfo(geneId: $gene) {
      id
      symbol
      description
      chromosome
      start
      end
    }
    studiesAndLeadVariantsForGeneByL2G(geneId: $gene) {
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

    qry$query(name = "getStudiesLeadL2G", x = query)

    # Execute the query
    output0 <- con$exec(qry$queries$getStudiesLeadL2G, variables)
    output1 <- jsonlite::fromJSON(output0, flatten = TRUE)

    if (length(output1$data$studiesAndLeadVariantsForGeneByL2G) != 0) {
      # Tidy the output
      output1$data$studiesAndLeadVariantsForGeneByL2G$ensembl_id <- rep(output1$data$geneInfo$id,
                                                                        length(output1$data$studiesAndLeadVariantsForGeneByL2G$yProbaModel))

      output1$data$studiesAndLeadVariantsForGeneByL2G$gene_symbol <- rep(output1$data$geneInfo$symbol,
                                                                         length(output1$data$studiesAndLeadVariantsForGeneByL2G$yProbaModel))

     final_output <- dplyr::bind_rows(final_output, output1$data$studiesAndLeadVariantsForGeneByL2G)

      if (!is.na(l2g)) {
        final_output <- final_output %>% dplyr::filter(yProbaModel >= l2g)
      }

      if (!is.na(pvalue)) {
        final_output <- final_output %>% dplyr::filter(pval <= pvalue)
      }

      final_output <- final_output %>% dplyr::mutate(across((yProbaModel:yProbaPathogenicity), ~ round(., 3)))

      if (!is.null(vtype)) {
        final_output <- final_output %>%
          dplyr::filter(variant.mostSevereConsequence %in% vtype)
      }
    }
  }

  if (nrow(final_output) != 0) {
    final_output$study.traitEfos <- as.character(final_output$study.traitEfos)
  }

  return(final_output)

}, error = function(e) {
  # Handling connection timeout
  if(grepl("Timeout was reached", e$message)) {
    stop("Connection timeout reached while connecting to the Open Targets Genetics GraphQL API.")
  } else {
    stop(e) # Handle other types of errors
  }
})
}
