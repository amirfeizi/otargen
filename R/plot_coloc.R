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
  dt0 <- test
  dt0$study.traitCategory <- base::tolower(dt0$Trait_reported)
  dt1 <- dt0 %>%
    dplyr::mutate(Trait_reported_trimmed = stringr::str_replace_all(Trait_reported, pattern = "[:punct:]|[:symbol:]", replacement = "")) %>%
    dplyr::mutate(Trait_reported_trimmed = stringr::str_trunc(Trait_reported_trimmed, width = 35, side = "right"))

  dt2 <- dt1[!duplicated(dt1[ , c("Study","Lead_variant","Molecular_trait")]),]


  dt3 <- dt2 %>% dplyr::group_by(Molecular_trait,Lead_variant) %>%
    dplyr::arrange(desc(dt2$`log2(H4/H3)`)) %>% dplyr::top_n(1) %>%
    dplyr::select(Trait_reported_trimmed, Molecular_trait,Lead_variant,
                  Study,Tissue, Source,`log2(H4/H3)`) %>% dplyr::ungroup()


  # source <- match.arg(source)
  # type <- match.arg(type)

  if (biobank == TRUE) {
    dt3 <- dt2 %>% dplyr::filter(grepl(pattern = "^GCST.*", Study))
  } else {
    dt3 <- dt2

  }

 p <- dt3 %>% ggplot2::ggplot(ggplot2::aes(x= reorder(Trait_reported_trimmed, -`log2(H4/H3)`),
                                            `log2(H4/H3)`, fill = Source)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::coord_flip () +
    ggplot2::facet_wrap(.~Molecular_trait) +

    ggplot2::geom_label(ggplot2::aes(
     label = Lead_variant,
     fill = Source),
   color = "white", fontface = "bold",  label.size = 0.1) +
    ggplot2::xlab("") +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank()
    )
  return(p)
}
