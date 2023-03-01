#' Plot the the scores obtained from the L2G model results
#'
#' @param data Data frame: result of studiesAndLeadVariantsForGeneByL2G function.
#' @param disease_efo String: input efo id to filter the L2G data for a particular disease.
#'
#' @return A radar plot for the input disease and the genes associated with that disease. The variables shown include L2G score, chromatin interaction, variant
#' pathogenicity and distance.
#'
#' @export
#'
#' @examples
#' studiesAndLeadVariantsForGeneByL2G(list("ENSG00000167207","ENSG00000096968","ENSG00000138821", "ENSG00000125255")) %>% plot_l2g(disease = "EFO_0003767")
#'

plot_l2g <- function(data, disease_efo=NULL){
  exclude <- c("phenotype", "measurement", "Uncategorised", "biological process")
  data <- dplyr::filter(data, !study.traitCategory %in% exclude)

  df <- data[,c("yProbaModel","yProbaDistance","yProbaInteraction","yProbaMolecularQTL","yProbaPathogenicity", "gene_symbol",
                "study.traitReported", "study.traitEfos", "study.traitCategory","pval")]

  df <- setNames(df, c("L2G_score","Distance","Interaction", "mQTL", "Pathogenicity", "Gene_name", "Traits",
                       "EFO_ID","Trait_category", "pval"))

  #df <- df[order(df$L2G_score,decreasing=TRUE),]


  if (!is.null(disease_efo)){
    df <- df %>% dplyr::filter(EFO_ID == disease_efo) %>% dplyr::group_by(Gene_name) %>% dplyr::filter(L2G_score == max(L2G_score)) %>% data.frame()
    df_data <- df[, 1:6]
    plot <- ggiraphExtra::ggRadar(data = df_data,mapping = ggplot2::aes(colour = Gene_name), rescale = FALSE,
                          use.label = TRUE, alpha = 0.12, size = 2, legend.position = "right") + ggplot2::labs(title = df[1,'Traits'])
  }
  else{
    df <- df %>% dplyr::group_by(Traits)%>% dplyr::arrange(dplyr::desc(L2G_score)) %>% head(n=3) %>% data.frame()
    df_data <- df[, 1:7]
    plot <- ggiraphExtra::ggRadar(data = df_data, mapping = ggplot2::aes(colour = Gene_name, facet=Traits),
                                   rescale = FALSE, use.label = TRUE, size = 2, alpha = 0.12, legend.position = "right")
  }
  return (plot)
}
