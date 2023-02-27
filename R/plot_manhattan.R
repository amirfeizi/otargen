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
#' manhattan(studyid = "GCST90002357") %>% plot_l2g(pval= 10e-8)
#'

plot_l2g <- function(data, pval=10e-8){
  data %>% filter(pval <= 10e-8)
  gwas_data <- data[c('variant.id', 'variant.position', 'variant.chromosome', 'pval')]
  manhplot <- ggplot(gwas_data, aes(x = variant.position, y = -log10(pval),
                                    color = as_factor(variant.chromosome), size = -log10(pval))) +
    geom_hline(yintercept = -log10(sig), color = "grey40", linetype = "dashed") +
    geom_point(alpha = 0.75) +
    scale_x_continuous(label = axis_set$chr, breaks = axis_set$center) +
    scale_y_continuous(expand = c(0,0), limits = c(0, ylim)) +
    scale_color_manual(values = rep(c("#276FBF", "#183059"), unique(length(axis_set$chr)))) +
    scale_size_continuous(range = c(0.5,3)) +
    labs(x = NULL,
         y = "-log<sub>10</sub>(p)") +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.title.y = element_markdown(),
      axis.text.x = element_text(angle = 60, size = 8, vjust = 0.5)
    )


}
