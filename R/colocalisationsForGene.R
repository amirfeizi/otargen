#' Retrieves Colocalisation Statistics for a Given Gene
#'
#' Retrieves Colocalisation statistics for a gene using ENSEMBL gene IDs or gene symbol.
#' Colocalisation analysis is performed between all studies in the Portal with at least one overlapping
#' associated locus. This analysis tests whether two independent associations at the same locus are
#' consistent with having a shared causal variant. The function supports multiple gene IDs as a list.
#' The returned data frame (tibble format) includes studies that have evidence of colocalisation
#' with molecular QTLs for the selected gene(s).
#'
#' @param genes Character: Gene ENSEMBL ID (e.g. ENSG00000169174) or gene symbol (e.g. PCSK9). Multiple gene IDs are supported as a character vector.
#'
#' @return A data frame (tibble format) including the colocalisation data for the query gene(s).
#'
#' The output tibble contains the following columns:
#' \itemize{
#'   \item{\code{Study}:} Character vector. Study identifier.
#'   \item{\code{Trait_reported}:} Character vector. Reported trait associated with the colocalisation.
#'   \item{\code{Lead_variant}:} Character vector. Lead variant associated with the colocalisation.
#'   \item{\code{Molecular_trait}:} Character vector. Molecular trait associated with the colocalisation.
#'   \item{\code{Gene_symbol}:} Character vector. Gene symbol associated with the colocalisation.
#'   \item{\code{Tissue}:} Character vector. Tissue where the colocalisation occurs.
#'   \item{\code{Source}:} Character vector. Source of the colocalisation data.
#'   \item{\code{H3}:} Numeric vector. H3 value associated with the colocalisation.
#'   \item{\code{log2_H4_H3}:} Numeric vector. Log2 ratio of H4 to H3 values.
#'   \item{\code{Title}:} Character vector. Title of the study.
#'   \item{\code{Author}:} Character vector. Author(s) of the study.
#'   \item{\code{Has_sumstats}:} Logical vector. Indicates if the study has summary statistics.
#'   \item{\code{numAssocLoci}:} Numeric vector. Number of associated loci in the study.
#'   \item{\code{nInitial_cohort}:} Numeric vector. Number of samples in the initial cohort.
#'   \item{\code{study_nReplication}:} Numeric vector. Number of samples in the replication cohort.
#'   \item{\code{study_nCases}:} Numeric vector. Number of cases in the study.
#'   \item{\code{Publication_date}:} Character vector. Publication date of the study.
#'   \item{\code{Journal}:} Character vector. Journal where the study was published.
#'   \item{\code{Pubmed_id}:} Character vector. PubMed identifier of the study.
#' }
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @import rlang
#'
#' @examples
#' \dontrun{
#' result1 <- colocalisationsForGene(gene = c("ENSG00000163946", "ENSG00000169174", "ENSG00000143001"))
#' result2 <- colocalisationsForGene(gene = "ENSG00000169174")
#' result3 <- colocalisationsForGene(gene = c("TP53", "TASOR"))
#' result4 <- colocalisationsForGene(gene = "TP53") }
#'
#' @export
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

colocal3 <- data.frame()

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
