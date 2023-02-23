#' Plot the colocalisation function results
#'
#' @param data Data Frame: result of colocalisationsForGene function in data frame format, contacting the phewas information for a variant id
#' @param disease Logical: TRUE and FALSE variable, with the default value of TRUE to filter the phewas data for disease.
#' @param source Character vector: choices for data sources of PHEWAS analysis including FINNGEN, GCST, NEAL (UKBioBANK), and SAGE.
#' @param ...
#'
#' @return A plot for colocalisation information
#'
#' @examples
#' pheWAS(variantid = "14_87978408_G_A") %>% plot_phewas(disease = TRUE)
#'
#' @export
#'
#'
plot_coloc <- function(data, biobank = FALSE) {
  dt0 <- data
  dt0$study.traitCategory <- base::tolower(dt0$Trait_reported)
  dt1 <- dt0 %>%
    dplyr::mutate(Trait_reported_trimmed = stringr::str_replace_all(Trait_reported, pattern = "[:punct:]|[:symbol:]", replacement = "")) %>%
    dplyr::mutate(Trait_reported_trimmed = stringr::str_trunc(Trait_reported_trimmed, width = 35, side = "right"))

  dt2 <- dt1[!duplicated(dt1[ , c("Study","Lead_variant","Molecular_trait")]),]


  # source <- match.arg(source)
  # type <- match.arg(type)

  if (biobank == TRUE) {
    dt3 <- dt2 %>% dplyr::filter(grepl(pattern = "^GCST.*", Study))
  } else {
    dt3 <- dt2
  }
  p <-  dt3 %>% ggplot2::ggplot(ggplot2::aes(Trait_reported,
                                                log2(H4/H3),
    color = Source, shape = Tissue
  )) +
    ggplot2::geom_point() +
    ggplot2::geom_jitter(width = 0.3, height = 0.3) +
    ggplot2::geom_label(
      ggplot2::aes(Trait_reported,
                   log2(H4/H3),
        label = Lead_variant
      ),
      data = dt3[dt3$`log2(H4/H3)` > 5, ],
      vjust = "inward", hjust = "inward"
    ) +
    ggplot2::scale_shape_manual(name = "beta direction", values = c(6, 2)) +
    ggplot2::scale_color_discrete(name = "Data source") +
    ggplot2::geom_hline(yintercept = 5, color = "grey", linetype = 2) +
    ggplot2::xlab("") +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  return(p)
}
