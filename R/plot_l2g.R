#' Plot the the scores obtained from the L2G model results
#'
#' @param data is the result of studiesAndLeadVariantsForGeneByL2G function in data frame format
#' @param disease_efo is the input efo id to filter the L2G data for a particular disease.
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

  df <- data[,c("yProbaModel","yProbaDistance","yProbaInteraction","yProbaPathogenicity", "yProbaMolecularQTL","gene_symbol",
                "study.traitReported", "study.traitEfos", "study.traitCategory","pval")]

  df <- setNames(df, c("L2G_score","Distance","Interaction", "Pathogenicity", "mQTL", "Gene_name", "Traits",
                       "EFO_ID","Trait_category", "pval"))

  df <- df[order(df$L2G_score,decreasing=TRUE),]


  if (!is.null(disease_efo)){
    df <- df %>% dplyr::filter(EFO_ID == disease_efo) %>% dplyr::group_by(Gene_name) %>% dplyr::filter(L2G_score == max(L2G_score)) %>% data.frame()
    names <- df$Gene_name
    rownames(df) <- names
    df <- tibble::rownames_to_column(df, "group")
    ggradar::ggradar(df[,1:6], values.radar = c(0, 0.5, 1), group.line.width = 1,background.circle.colour = "grey",
                     group.point.size = 2, legend.position = "bottom", plot.title=df[1,'Traits'])
  }
  else{
    df <- df %>% dplyr::group_by(Gene_name) %>% dplyr::filter(L2G_score == max(L2G_score)) %>% data.frame()
    names <- df$Traits
    rownames(df) <- names
    df <- tibble::rownames_to_column(df, "group")
    df <- df %>% dplyr::mutate(group = paste(group, Gene_name, sep=": "))
    ggradar::ggradar(df[,1:6], values.radar = c(0, 0.5, 1), group.line.width = 1,background.circle.colour = "grey",
                     group.point.size = 2, legend.position = "bottom") + ggplot2::facet_wrap(~group) + ggplot2::theme_light()
  }

}
