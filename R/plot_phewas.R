#' Plot the PheWAS function results
#'
#' @param data is the result of PheWAS function in data frame format, contacting the phewas information for a variand id
#' @param disease is a logical TRUE and FALSE variable, with the default value of TRUE to filter the phewas data for disease.
#' @param source is a character vector of choices for data sources of phewas analyis including FINNGEN, GCST, NEAL (UKbioBANK), and SAGE
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' pheWAS(variantid = "14_87978408_G_A") %>% plot_phewas(disease = TRUE)
#'
plot_phewas <- function(data, disease = TRUE,  source = c("GCST","FINNGEN","NEALE","SAIGE")){

  dt0 <- data
  dt0$study.traitCategory <- base::tolower(dt0$study.traitCategory)
  dt1 <- dt0  %>%  dplyr::mutate(study.traitReported_trimmed = stringr::str_replace_all(study.traitReported,pattern = "[:punct:]|[:symbol:]",replacement = "")) %>%
    dplyr::mutate(study.traitReported_trimmed = stringr::str_trunc(study.traitReported_trimmed, width = 35,side = "right"))


  #source <- match.arg(source)
  #type <- match.arg(type)

  if (disease ) {

    dt2 <- dt1 %>% dplyr::filter(study.source %in% source) %>%
      dplyr::filter(!study.traitCategory %in% c("measurement", "phenotype", "biological process","uncategorised")) %>%
      dplyr::mutate(beta_shape = ifelse(beta>0, "positive","negetive"))


  } else {

  dt2 <- dt1 %>% dplyr::filter(study.source %in% source) %>%
    dplyr::filter(study.traitCategory %in% c("measurement", "phenotype", "biological process","uncategorised")) %>%
    dplyr::mutate(beta_shape = ifelse(beta>0, "positive","negetive"))
  }

   ggplot2::ggplot(data = dt2, ggplot2::aes(study.traitCategory,
      -log10(pval), color = study.source, shape = beta_shape)) +
    ggplot2::geom_point()+
   ggplot2::geom_jitter( width = 0.3, height = 0.3) +

    ggplot2::geom_label(ggplot2::aes(study.traitCategory, -log10(pval),
                   label = study.traitReported_trimmed),
                    data = dt2[-log10(dt2$pval)>5 ,],
              vjust="inward",hjust="inward") +

    ggplot2::scale_shape_manual(name = "beta direction" ,values = c(6,2)) +
    ggplot2::scale_color_discrete(name = "Data source") +
    ggplot2::geom_hline(yintercept = 5, color = "grey", linetype = 2) +
    ggplot2::xlab("") +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))


}