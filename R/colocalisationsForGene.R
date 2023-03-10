#' Retrieves colocalisation data for a gene.
#'
#' This functions takes gene(s)/ensemble id(s) as an input and returns a tibble
#' format data table of studies that have evidence of colocalisation with molecular QTLs.
#' The output table includes the following columns-
#' Study, Trait_reported, Lead_variant, Molecular_trait, Gene_symbol,
#' Tissue, Source, H3, H4, log2(H4/H3), Title, Author, Has_sumstats,
#' numAssocLoci, nInitial, cohort, study_nReplication, study_nCases,
#' Publication_date, Journal, and Pubmed_id.
#'
#' @param genes String: one or more gene ENSEMBL identifier or gene name.
#' @import dplyr
#' @importFrom magrittr %>%
#' @import rlang
#'
#' @return a tibble including the queries gene(s) colocalisation data
#'
#'
#' @examples
#' \dontrun{
#' otargen::colocalisationsForGene(genes=list("ENSG00000163946",
#' "ENSG00000169174", "ENSG00000143001"))
#' otargen::colocalisationsForGene(genes="ENSG00000169174")
#' otargen::colocalisationsForGene(genes=list("TP53", "TASOR"))
#' otargen::colocalisationsForGene(genes="TP53")
#' }
#' @export
#'
#'
colocalisationsForGene <- function(genes) {

  cli::cli_progress_step("Connecting the database...", spinner = TRUE)
  con <- ghql::GraphqlClient$new("https://api.genetics.opentargets.org/graphql")
  qry <- ghql::Query$new()

  #Query for gene name search
  # check for gene name:
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
  colocal2 <- colocal2 %>% dplyr::select(study.studyId, study.traitReported,
                                         leftVariant.id, gene_symbol, gene_id, tissue.name, qtlStudyId,
                                         h3, h4, log2h4h3, study.pubTitle, study.pubAuthor, study.hasSumstats,
                                         study.numAssocLoci, study.nInitial, study.nReplication, study.nCases,
                                         study.pubDate, study.pubJournal, study.pmid) %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ round(., 2)))

  colnames(colocal2) <- c("Study", "Trait_reported", "Lead_variant", "Molecular_trait", "Gene_symbol",
                          "Tissue", "Source", "H3", "H4", "log2(H4/H3)", "Title", "Author", "Has_sumstats", "numAssocLoci",
                          "nInitial cohort", "study_nReplication", "study_nCases", "Publication_date", "Journal", "Pubmed_id")

  colocal3 <- colocal2 %>% dplyr::arrange(dplyr::desc(`log2(H4/H3)`)) %>% dplyr::tibble()
}

if (nrow(colocal3) == 0) {
  colocal3 <- data.frame()
}

return(colocal3)
}
