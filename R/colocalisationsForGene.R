#' Retrieves colocalisation data for a gene.
#'
#' This functions takes a (list) gene ensembl id(s) as an input and retrieves datatable for studies that have evidence of colocalisation with molecular QTLs. The output table includes the following columns-
#' Study, trait_reported, Lead_variant, Molecular_trait, Gene_symbol,
#' Tissue, Source, H3, H4, log2(H4/H3), Title, Author, Has_sumstats,
#' numAssocLoci, nInitial, cohort, study_nReplication, study_nCases,
#' Publication_date, Journal, and Pubmed_id.
#'
#' @param ensembl_ids String: one or more gene ENSEMBL identifier.
#' @param h4 Float: This is cut off for log2(H4/H3) which measures evidence for colocalisation between 2 traits. The default cut off is 5.
#'
#' @return Data frame including the queried gene identity and its colocalisation data
#'
#' @examples
#' colocalisationsForGene(ensembl_ids = list("ENSG00000163946", "ENSG00000169174", "ENSG00000143001"))
#' or
#' colocalisationsForGene(ensembl_ids = "ENSG00000169174")
#'
#' @export
#'
#'
colocalisationsForGene <- function(ensembl_ids, h4 = 5) {
  # Check ensembl id format

  if (length(ensembl_ids) == 1) {
    if (!grepl(pattern = "ENSG\\d{11}", ensembl_ids)) {
      stop("\n Please provide Ensemble gene ID")
    }
  } else {
    for (i in ensembl_ids) {
      if (!grepl(pattern = "ENSG\\d{11}", i)) {
        stop("\n Please provide Ensemble gene ID")
      }
    }
  }

  ensembl_ids <- ensembl_ids

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
    studyId
    pmid
    pubDate
    pubJournal
    pubTitle
    pubAuthor
    hasSumstats
    nInitial
    nReplication
    nCases
    traitReported
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
  cli::cli_progress_step("Connecting the database...", spinner = TRUE)
  con <- ghql::GraphqlClient$new("https://api.genetics.opentargets.org/graphql")
  qry <- ghql::Query$new()

  for (input_gene in ensembl_ids) {
    cli::cli_progress_step(paste0("Downloading data for ", input_gene, " ..."), spinner = TRUE)


    variables <- list(gene = input_gene)

    qry$query(name = "getgenColocal", x = query)

    colocal <- con$exec(qry$queries$getgenColocal, variables)
    colocal1 <- jsonlite::fromJSON(colocal, flatten = TRUE)

    colocal_genes_info <- dplyr::bind_rows(colocal_genes_info, as.data.frame(colocal1$data$geneInfo))

    colocal1$data$colocalisationsForGene$gene_symbol <- rep(
      colocal1$data$geneInfo$symbol,
      length(colocal1$data$colocalisationsForGene$phenotypeId)
    )

    colocal1$data$colocalisationsForGene$gene_id <- rep(
      colocal1$data$geneInfo$id,
      length(colocal1$data$colocalisationsForGene$phenotypeId)
    )

    colocal2 <- dplyr::bind_rows(colocal2, as.data.frame(colocal1$data$colocalisationsForGene))
    cli::cli_progress_update()
  }

  if (nrow(colocal2) != 0) {
    colocal2 <- colocal2 %>%
      dplyr::select(
        study.studyId, study.traitReported, leftVariant.id, gene_symbol, gene_id, tissue.name, qtlStudyId,
        h3, h4, log2h4h3, study.pubTitle, study.pubAuthor, study.hasSumstats, study.numAssocLoci, study.nInitial,
        study.nReplication, study.nCases, study.pubDate, study.pubJournal, study.pmid
      ) %>%
      dplyr::mutate(across(where(is.numeric), ~ round(., 2)))

    colnames(colocal2) <- c(
      "Study", "Trait_reported", "Lead_variant", "Molecular_trait", "Gene_symbol",
      "Tissue", "Source", "H3", "H4", "log2(H4/H3)", "Title", "Author", "Has_sumstats", "numAssocLoci",
      "nInitial cohort", "study_nReplication", "study_nCases", "Publication_date", "Journal", "Pubmed_id"
    )

    colocal3 <- colocal2 %>%
      dplyr::filter(log2(H4 / H3) >= h4) %>%
      dplyr::arrange(desc(log2(H4 / H3)))
  }

  return(colocal3)
}
