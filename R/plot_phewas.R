#' Plot \code{PheWAS()} results.
#'
#' This plot visualizes which traits are associated with the user's selected variant in the UK Biobank,
#' FinnGen, and/or GWAS Catalog summary statistics repository based on PheWAS analysis.
#' The associated traits are mapped onto the \emph{x-axis}, and their corresponding -log10(p-value) values are plotted on the \emph{y-axis}.
#' A horizontal line is shown at a p-value cutoff of 0.005 to indicate significant associations.
#' Associations above this cutoff are labeled with the trait's name, and the sources of the associations are color-coded as points.
#'
#' @param data Data Frame: The result of the \code{PheWAS()} function in data frame format, containing the PheWAS information for a selected variant ID.
#' @param disease Logical: A logical variable indicating whether to filter the PheWAS data for disease (default: TRUE).
#' @param source Character vector: Choices for the data sources of PheWAS analysis, including FINNGEN, GCST, NEALE (UKBiobank), and SAIGE.
#'
#' @return A plot to prioritize variants based on their -log10(p-value).
#'
#' @examples
#' \dontrun{
#' p <- pheWAS(variant_id = "14_87978408_G_A") %>%
#'      plot_phewas(disease = TRUE)
#' p
#' }
#' @import dplyr
#' @import ggplot2
#' @importFrom magrittr %>%
#' @export
#'
#'
plot_phewas <- function(data, disease = TRUE, source = c("GCST", "FINNGEN", "NEALE", "SAIGE")) {
  # Prepare the data
  dt0 <- data
  dt0$study.traitCategory <- tolower(dt0$study.traitCategory)
  dt1 <- dt0 %>%
    mutate(study.traitReported_trimmed = stringr::str_replace_all(study.traitReported, pattern = "[:punct:]|[:symbol:]", replacement = "")) %>%
    mutate(study.traitReported_trimmed = stringr::str_trunc(study.traitReported_trimmed, width = 35, side = "right"))

  # Filter and categorize the data
  if (disease) {
    dt2 <- dt1 %>%
      filter(study.source %in% source) %>%
      filter(!study.traitCategory %in% c("measurement", "phenotype", "biological process", "uncategorized")) %>%
      mutate(beta_shape = ifelse(beta > 0, "positive", "negative"))
  } else {
    dt2 <- dt1 %>%
      filter(study.source %in% source) %>%
      filter(study.traitCategory %in% c("measurement", "phenotype", "biological process", "uncategorized")) %>%
      mutate(beta_shape = ifelse(beta > 0, "positive", "negative"))
  }

  # Create the plot
  p <- ggplot(data = dt2, aes(study.traitCategory, -log10(pval), color = study.source, shape = beta_shape)) +
    geom_point() +
    geom_jitter(width = 0.3, height = 0.3) +
    geom_label(
      aes(study.traitCategory, -log10(pval), label = study.traitReported_trimmed),
      data = dt2[-log10(dt2$pval) > 5, ],
      vjust = "inward", hjust = "inward"
    ) +
    scale_shape_manual(name = "beta direction", values = c(6, 2)) +
    scale_color_discrete(name = "Data source") +
    geom_hline(yintercept = 5, color = "grey", linetype = 2) +
    xlab("") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  return(p)
}
