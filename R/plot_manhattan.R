#' Plot the the scores obtained from the manhattan associations.
#'
#' @param data is the result of manhattan function in data frame format
#' @param pval
#'
#' @return A plot
#'
#' @export
#'
#' @examples
#' manhattan(studyid = "GCST003044") %>% plot_l2g(pval= 10e-8)
#'

plot_l2g <- function(data, pvalue=10e-8){
  data %>% filter(pval <= pvalue)
  gwasResults <- data[c('variant.position', 'variant.chromosome', 'pval', 'variant.id')] %>% unique()
  gwasResults <- gwasResults %>%  dplyr::rename("Positions"="variant.position", "CHR"="variant.chromosome", "P"="pval", "SNP"="variant.id")
  gwasResults$text <- paste("SNP: ", gwasResults$SNP, "\nPosition: ", gwasResults$Positions, "\nChromosome: ", gwasResults$CHR, "\nLOD score:", -log10(gwasResults$P) %>% round(2), sep="")
  print (gwasResults)
  axisdf <- gwasResults %>% dplyr::group_by(CHR) %>% dplyr::summarize(center=( max(Positions) + min(Positions) ) / 2 )
  # Make the plot
  p <- ggplot2::ggplot(gwasResults, ggplot2::aes(x=Positions, y=-log10(P), text=text)) +

    # Show all points
    ggplot2::geom_point(ggplot2::aes(color=CHR)) +
    ggplot2::scale_color_manual(values = rep(c("darkblue", "darkgreen"))) +

    # custom X axis:
    ggplot2::scale_x_continuous(label = axisdf$CHR, breaks= axisdf$center) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +

    # Custom the theme:
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position="right", panel.border = ggplot2::element_blank(), panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank())

  plotly::ggplotly(p, tooltip="text")

}
