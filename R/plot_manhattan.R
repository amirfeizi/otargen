#' Plot the data obtained from the manhattan associations.
#'
#' The plot is generated using the p-value and variant information obtained from
#' the manhattan function. Users can provide p-value as a parameter to obtain a plot with the significant SNPs.
#'
#'
#' @param data Data frame: result of the manhattan function containing SNP information
#'
#' @return A manhattan plot representing SNPs on the x-axis with the corresponding
#' negative logarithm of p-value on the y-axis.
#'
#' @examples
#' \dontrun{
#' otargen::manhattan(studyid = "GCST003044") %>% otargen::plot_manhattan()
#'}
#' @export
#'

plot_manhattan <- function(data){

  gwasResults <- data[c('variant_position', 'variant_chromosome', 'pval', 'variant_id',
                        'best_locus2genes_score', 'best_locus2genes_gene_symbol')] %>% unique() %>%
                         dplyr::rename("BP"="variant_position", "CHR"="variant_chromosome",
                                               "P"="pval", "SNP"="variant_id")

  gwasResults$CHR = as.integer(gwasResults$CHR)

  gwasResults <- gwasResults %>% dplyr::group_by(CHR) %>% dplyr::summarise(chr_len=max(BP)) %>%
                 dplyr::mutate(tot=cumsum(as.numeric(chr_len))-chr_len) %>%
                 dplyr::select(-chr_len) %>% dplyr::left_join(gwasResults, ., by=c("CHR"="CHR")) %>%
                 dplyr::arrange(CHR, BP) %>% dplyr::mutate(BPcum=BP+tot)

  df_axis <- gwasResults %>% dplyr::group_by(CHR) %>% dplyr::summarize(center=( max(BPcum) + min(BPcum))/2)

  l2g_annot <- gwasResults  %>% dplyr::group_by(CHR)%>% dplyr::arrange(dplyr::desc(best_locus2genes_score)) %>%
    dplyr::slice(1:3)%>% as.data.frame()

  p_cutoff <- 10e-8
  # Make the plot
  plt <- ggplot2::ggplot(gwasResults, ggplot2::aes(x=BPcum, y=-log10(P))) +

    #ggplot2::geom_label() +
    ggrepel::geom_label_repel(data=l2g_annot, ggplot2::aes(label=best_locus2genes_gene_symbol, color=as.factor(CHR)), na.rm=TRUE) +

    ggplot2::geom_point(ggplot2::aes(color=as.factor(CHR)), alpha=0.85, size=1.3) +
    ggplot2::scale_color_manual(values = rep(c("darkblue", "darkgreen"), unique(length(df_axis$CHR)))) +
    ggplot2::scale_size_continuous(range = c(1,25)) +
    ggplot2::scale_x_continuous(name= 'Chromosome', label = df_axis$CHR, breaks=df_axis$center) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position="none",
      panel.border = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank()
    )
  return (plt)
}
