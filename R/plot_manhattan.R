#' Plot the the scores obtained from the manhattan associations.
#'
#' @param data is the result of manhattan function in data frame format
#' @param pval
#'
#' @return A manhattan plot
#'
#' @export
#'
#' @examples
#' manhattan(studyid = "GCST003044") %>% plot_l2g(pval= 10e-8)
#'

plot_l2g <- function(data, pvalue=10e-8){

  gwasResults <- data[c('variant.position', 'variant.chromosome', 'pval', 'variant.id')] %>% unique() %>%
            dplyr::filter(pval <= pvalue)

  gwasResults <- gwasResults %>% dplyr::rename("BP"="variant.position", "CHR"="variant.chromosome",
                                               "P"="pval", "SNP"="variant.id")

  gwasResults$CHR = as.integer(gwasResults$CHR)

  # Compute chromosome size
  gwasResults <- gwasResults %>% dplyr::group_by(CHR) %>%
    dplyr::summarise(chr_len=max(BP)) %>%

    # Calculate cumulative position of each chromosome
    dplyr::mutate(tot=cumsum(as.numeric(chr_len))-chr_len) %>%
    dplyr::select(-chr_len) %>%

    # Add this info to the initial dataset
    dplyr::left_join(gwasResults, ., by=c("CHR"="CHR")) %>%

    # Add a cumulative position of each SNP
    dplyr::arrange(CHR, BP) %>%
    dplyr::mutate( BPcum=BP+tot)

  axisdf <- gwasResults %>% dplyr::group_by(CHR) %>% dplyr::summarize(center=( max(BPcum) + min(BPcum))/2)

  gwasResults$text <- paste("SNP: ", gwasResults$SNP, "\nPosition: ", gwasResults$BP, "\nChromosome: ",
                            gwasResults$CHR, "\n-log10(pval):", -log10(gwasResults$P) %>% round(3), sep="")

  # Make the plot
  p <- ggplot2::ggplot(gwasResults, ggplot2::aes(x=BPcum, y=-log10(P), text=text)) +

    # Show all points
    ggplot2::geom_point(ggplot2::aes(color=as.factor(CHR)), alpha=0.8, size=1.3) +
    ggplot2::scale_color_manual(values = rep(c("darkblue", "darkgreen"), 22)) +

    # custom X axis:
    ggplot2::scale_x_continuous(name= 'Chromosome', label = axisdf$CHR, breaks=axisdf$center) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +

    # Custom the theme:
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position="none",
      panel.border = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank()
    ) +
    ggplot2::labs(title = 'Manhattan Plot')


  plotly::ggplotly(p, tooltip="text")

}
