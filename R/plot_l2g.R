#' Radar plot for L2G partial scores from \code{studiesAndLeadVariantsForGeneByL2G()}
#'
#' This function returns a radar plot to compare the partial scores,
#' important for prioritising the causal genes that are obtained
#' from the \code{studiesAndLeadVariantsForGeneByL2G()} function. The user can
#' decide to plot only for a specific disease by specifying an \code{EFO} ID
#' for the \code{disease} argument, otherwise the returned plot will will facet
#' based on existing traits/diseases in the outputs from \code{studiesAndLeadVariantsForGeneByL2G()}.
#'
#'
#' @param data Data frame: result of \code{studiesAndLeadVariantsForGeneByL2G} function.
#' @param disease_efo Character: Input EFO id to filter the L2G data for a particular disease.
#' @param l2g_cutoff Numeric: Sets the minimum L2G score threshold for diseases to be considered in the plot.
#' @param top_n_disease Numeric: Determines the number of top diseases to plot for each gene, ranked by L2G score.
#' @return A radar plot for the input disease and the genes associated with that disease.
#' The variables shown include L2G score, chromatin interaction, variant pathogenicity and distance.
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' \dontrun{
#' p <- studiesAndLeadVariantsForGeneByL2G(list("ENSG00000167207","ENSG00000096968",
#'   "ENSG00000138821", "ENSG00000125255")) %>% plot_l2g(disease = "EFO_0003767")
#' p
#'}
#'

plot_l2g <- function(data, disease_efo = NULL, l2g_cutoff= 0.5, top_n_disease = 1) {
  # Exclude irrelevant trait categories
  exclude <- c("phenotype", "measurement", "Uncategorised", "biological process")
  data <- dplyr::filter(data, !study.traitCategory %in% exclude)

  # Select relevant columns and rename them
  df <- data %>%
    dplyr::select(yProbaModel, yProbaDistance, yProbaInteraction, yProbaMolecularQTL, yProbaPathogenicity, gene_symbol,
                  study.traitReported, study.traitEfos, study.traitCategory, pval) %>%
    dplyr::rename(L2G_score = yProbaModel,
                  Distance = yProbaDistance,
                  Interaction = yProbaInteraction,
                  mQTL = yProbaMolecularQTL,
                  Pathogenicity = yProbaPathogenicity,
                  Gene_name = gene_symbol,
                  Traits = study.traitReported,
                  EFO_ID = study.traitEfos,
                  Trait_category = study.traitCategory,
                  pval = pval)

  if (!is.null(disease_efo)) {
    # Filter by disease EFO ID and select top-scoring gene for each trait
    df <- df %>%
      dplyr::filter(EFO_ID == disease_efo) %>%
      dplyr::group_by(Gene_name) %>%
      dplyr::filter(L2G_score == max(L2G_score)) %>%
      dplyr::ungroup() %>%
      data.frame()
    df_data <- df[, 1:6]

    # Generate radar plot with title based on the first trait
    plot <- ggiraphExtra::ggRadar(data = df_data,
                                  mapping = ggplot2::aes(colour = Gene_name),
                                  rescale = FALSE,
                                  use.label = TRUE,
                                  alpha = 0.12,
                                  size = 2,
                                  legend.position = "right") +
      ggplot2::labs(title = df[1, "Traits"])
  } else {
    # Select top-scoring genes for each trait and plot in separate panels
    df <- df %>%
      dplyr::group_by(Gene_name) %>%
      dplyr::filter(L2G_score >= l2g_cutoff) %>%
      dplyr::arrange(dplyr::desc(L2G_score)) %>%
      dplyr::slice_head(n=top_n_disease) %>%
      dplyr::ungroup() %>%
      data.frame()
    df_data <- df[, 1:7]

    # Generate radar plot with facetting by traits
    plot <- ggiraphExtra::ggRadar(data = df_data,
                                  mapping = ggplot2::aes(colour = Gene_name, facet = Traits),
                                  rescale = FALSE,
                                  use.label = TRUE,
                                  size = 2,
                                  alpha = 0.12,
                                  legend.position = "right")
  }
  return(plot)
}
