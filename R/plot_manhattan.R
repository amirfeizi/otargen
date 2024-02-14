#' Plot results from \code{manhattan()}
#'
#' This function generates a Manhattan plot using the statistical summary data
#' obtained from the \code{manhattan()} function. Top 3 genes (based on p-value) are annotated per chromosome.
#'
#' @param data Data frame containing the necessary columns from \code{manhattan()} output for plotting:
#'   - \code{variant_position}: Variant position
#'   - \code{variant_chromosome}: Variant chromosome
#'   - \code{pval}: P-value
#'   - \code{variant_id}: Variant ID
#'   - \code{best_locus2genes_score}: Best locus2genes score
#'   - \code{best_locus2genes_gene_symbol}: Best locus2genes gene symbol
#'
#' @return A Manhattan plot visualizing the GWAS results.
#'
#' @examples
#' \dontrun{
#' p <- manhattan(study_id = "GCST003044") %>% plot_manhattan()
#'
#' p
#'}
#' @import dplyr
#' @import ggplot2
#' @importFrom magrittr %>%
#' @export



plot_manhattan <- function(data) {
  # Extract relevant columns and rename them
  gwasResults <- data %>%
    select(variant_position, variant_chromosome, pval, variant_id,
           best_locus2genes_score, best_locus2genes_gene_symbol) %>%
    unique() %>%
    rename(BP = variant_position, CHR = variant_chromosome,
           P = pval, SNP = variant_id)

  # Convert CHR column to integer
  gwasResults$CHR <- as.integer(gwasResults$CHR)

  # Calculate cumulative positions for each chromosome
  gwasResults <- gwasResults %>%
    group_by(CHR) %>%
    summarise(chr_len = max(BP)) %>%
    mutate(tot = cumsum(as.numeric(chr_len)) - chr_len) %>%
    select(-chr_len) %>%
    left_join(gwasResults, ., by = c("CHR" = "CHR")) %>%
    arrange(CHR, BP) %>%
    mutate(BPcum = BP + tot)

  # Calculate center positions for each chromosome for axis labels
  df_axis <- gwasResults %>%
    group_by(CHR) %>%
    summarize(center = (max(BPcum) + min(BPcum)) / 2)

  # Select top 3 annotations per chromosome based on locus2genes scores
  l2g_annot <- gwasResults %>%
    group_by(CHR) %>%
    arrange(desc(best_locus2genes_score)) %>%
    slice(1:3) %>%
    as.data.frame()

  # Set p-value cutoff
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
