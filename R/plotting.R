# ============================================================================
# Plotting functions for otargen query outputs
# ============================================================================

#' Plot adverse events for a drug as a lollipop chart.
#'
#' Displays the top adverse events ranked by log-likelihood ratio (logLR)
#' with a dashed reference line at the critical value threshold.
#'
#' @param df A tibble returned by \code{\link{adverseEventsQuery}}.
#' @param top_n Integer: number of top events to display (default: 20).
#'
#' @return A \code{ggplot} object.
#' @examples
#' \dontrun{
#' ae <- adverseEventsQuery(chemblId = "CHEMBL941")
#' plot_adverse_events(ae)
#' }
#' @importFrom ggplot2 ggplot aes geom_segment geom_point geom_vline
#'   scale_color_gradient labs theme_minimal theme element_text
#' @export
plot_adverse_events <- function(df, top_n = 20) {
  if (is.null(df) || nrow(df) == 0) stop("Input data frame is empty or NULL.")
  required <- c("name", "logLR", "criticalValue")
  if (!all(required %in% names(df))) {
    stop("Input must be output of adverseEventsQuery(). Missing columns: ",
         paste(setdiff(required, names(df)), collapse = ", "))
  }

  # Keep top N events by logLR
  df <- df[order(-df$logLR), ]
  if (nrow(df) > top_n) df <- df[seq_len(top_n), ]
  crit <- df$criticalValue[1]

  # Order factor levels by logLR for plotting
  df$name <- factor(df$name, levels = rev(df$name))

  ggplot2::ggplot(df, ggplot2::aes(x = logLR, y = name)) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = logLR, yend = name),
                          color = "grey60", linewidth = 0.4) +
    ggplot2::geom_point(ggplot2::aes(color = logLR), size = 3) +
    ggplot2::geom_vline(xintercept = crit, linetype = "dashed",
                        color = "firebrick", linewidth = 0.6) +
    ggplot2::scale_color_gradient(low = "steelblue", high = "firebrick") +
    ggplot2::labs(
      title = paste("Adverse Events (top", min(top_n, nrow(df)), ")"),
      subtitle = paste("Dashed line = critical value (", round(crit, 2), ")"),
      x = "Log-likelihood ratio (logLR)", y = NULL, color = "logLR"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 9))
}


#' Plot protein interaction network for a gene.
#'
#' Draws a circular network layout of molecular interaction partners
#' with edge thickness proportional to the interaction confidence score.
#'
#' @param df A tibble returned by \code{\link{interactionsQuery}}.
#' @param top_n Integer: number of top interactions to display (default: 20).
#'
#' @return A \code{ggplot} object.
#' @examples
#' \dontrun{
#' int <- interactionsQuery(ensgId = "ENSG00000141510",
#'   sourceDatabase = "intact", size = 25)
#' plot_interactions(int)
#' }
#' @importFrom ggplot2 ggplot aes geom_segment geom_point geom_text
#'   coord_equal labs theme_void theme element_text
#' @export
plot_interactions <- function(df, top_n = 20) {
  if (is.null(df) || nrow(df) == 0) stop("Input data frame is empty or NULL.")
  required <- c("targetA.approvedSymbol", "targetB.approvedSymbol", "score")
  if (!all(required %in% names(df))) {
    stop("Input must be output of interactionsQuery(). Missing columns: ",
         paste(setdiff(required, names(df)), collapse = ", "))
  }

  # Keep top N by score
  df <- df[order(-df$score), ]
  if (nrow(df) > top_n) df <- df[seq_len(top_n), ]

  # Collect unique node labels
  nodes <- unique(c(df$targetA.approvedSymbol, df$targetB.approvedSymbol))
  n <- length(nodes)

  # Circular layout coordinates
  angles <- seq(0, 2 * pi, length.out = n + 1)[seq_len(n)]
  coords <- data.frame(
    label = nodes,
    x = cos(angles),
    y = sin(angles),
    stringsAsFactors = FALSE
  )

  # Build edge data frame
  edges <- data.frame(
    x    = coords$x[match(df$targetA.approvedSymbol, coords$label)],
    y    = coords$y[match(df$targetA.approvedSymbol, coords$label)],
    xend = coords$x[match(df$targetB.approvedSymbol, coords$label)],
    yend = coords$y[match(df$targetB.approvedSymbol, coords$label)],
    score = df$score,
    stringsAsFactors = FALSE
  )

  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = edges,
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend, linewidth = score),
      color = "grey50", alpha = 0.5
    ) +
    ggplot2::geom_point(data = coords, ggplot2::aes(x = x, y = y),
                        size = 5, color = "steelblue") +
    ggplot2::geom_text(data = coords, ggplot2::aes(x = x * 1.12, y = y * 1.12,
                        label = label), size = 3.2) +
    ggplot2::coord_equal() +
    ggplot2::labs(title = "Protein Interaction Network", linewidth = "Score") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
}


#' Plot locus-to-gene prediction scores.
#'
#' Displays candidate genes ranked by their L2G score as a horizontal
#' bar chart with a color gradient.
#'
#' @param df A tibble returned by \code{\link{locus2GeneQuery}}.
#' @param top_n Integer: number of top genes to display (default: 15).
#'
#' @return A \code{ggplot} object.
#' @examples
#' \dontrun{
#' l2g <- locus2GeneQuery(studyLocusId = "fa375739ca2a6b825ce5cc69d117e84b")
#' plot_l2g(l2g)
#' }
#' @importFrom ggplot2 ggplot aes geom_col scale_fill_gradient
#'   labs theme_minimal theme element_text
#' @export
plot_l2g <- function(df, top_n = 15) {
  if (is.null(df) || nrow(df) == 0) stop("Input data frame is empty or NULL.")
  required <- c("target.approvedSymbol", "score")
  if (!all(required %in% names(df))) {
    stop("Input must be output of locus2GeneQuery(). Missing columns: ",
         paste(setdiff(required, names(df)), collapse = ", "))
  }

  # Keep top N by score
  df <- df[order(-df$score), ]
  if (nrow(df) > top_n) df <- df[seq_len(top_n), ]
  df$target.approvedSymbol <- factor(df$target.approvedSymbol,
                                     levels = rev(df$target.approvedSymbol))

  ggplot2::ggplot(df, ggplot2::aes(x = score, y = target.approvedSymbol,
                                   fill = score)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::scale_fill_gradient(low = "lightblue", high = "darkblue") +
    ggplot2::labs(
      title = "Locus-to-Gene (L2G) Predictions",
      x = "L2G Score", y = NULL, fill = "Score"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10))
}


#' Plot GWAS colocalisation results.
#'
#' Scatter plot of the H4 posterior probability (shared causal variant)
#' against the number of colocalising variants, with labels for each
#' colocalising trait and a threshold reference line at H4 = 0.8.
#'
#' @param df A data frame returned by \code{\link{gwasColocalisation}}.
#' @param h4_threshold Numeric: H4 threshold line to draw (default: 0.8).
#'
#' @return A \code{ggplot} object.
#' @examples
#' \dontrun{
#' coloc <- gwasColocalisation(study_locus_id = "5a86bfd40d2ebecf6ce97bbe8a737512")
#' plot_colocalisation(coloc)
#' }
#' @importFrom ggplot2 ggplot aes geom_point geom_hline geom_text
#'   scale_color_viridis_c labs theme_minimal theme element_text
#' @export
plot_colocalisation <- function(df, h4_threshold = 0.8) {
  if (is.null(df) || nrow(df) == 0) stop("Input data frame is empty or NULL.")
  required <- c("h4", "numberColocalisingVariants", "study.traitReported")
  if (!all(required %in% names(df))) {
    stop("Input must be output of gwasColocalisation(). Missing columns: ",
         paste(setdiff(required, names(df)), collapse = ", "))
  }

  # Truncate long trait names for readability
  df$trait_label <- ifelse(nchar(df$study.traitReported) > 35,
                           paste0(substr(df$study.traitReported, 1, 32), "..."),
                           df$study.traitReported)

  ggplot2::ggplot(df, ggplot2::aes(x = numberColocalisingVariants, y = h4)) +
    ggplot2::geom_hline(yintercept = h4_threshold, linetype = "dashed",
                        color = "firebrick", linewidth = 0.5) +
    ggplot2::geom_point(ggplot2::aes(color = h4), size = 3, alpha = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = trait_label),
                       size = 2.5, hjust = -0.05, vjust = -0.5,
                       check_overlap = TRUE) +
    ggplot2::scale_color_viridis_c(option = "plasma") +
    ggplot2::labs(
      title = "GWAS Colocalisation",
      subtitle = paste("Dashed line = H4 threshold (", h4_threshold, ")"),
      x = "Number of colocalising variants", y = "H4 posterior", color = "H4"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 13))
}


#' Plot drug indications by clinical stage.
#'
#' Horizontal bar chart of disease indications colored by the maximum
#' clinical trial stage reached (Phase 0 through Phase 4).
#'
#' @param df A tibble returned by \code{\link{indicationsQuery}}.
#' @param top_n Integer: number of top indications to display (default: 25).
#'
#' @return A \code{ggplot} object.
#' @examples
#' \dontrun{
#' ind <- indicationsQuery(chemblId = "CHEMBL941")
#' plot_indications(ind)
#' }
#' @importFrom ggplot2 ggplot aes geom_col scale_fill_manual
#'   labs theme_minimal theme element_text element_blank
#' @export
plot_indications <- function(df, top_n = 25) {
  if (is.null(df) || nrow(df) == 0) stop("Input data frame is empty or NULL.")
  required <- c("disease.name", "maxClinicalStage")
  if (!all(required %in% names(df))) {
    stop("Input must be output of indicationsQuery(). Missing columns: ",
         paste(setdiff(required, names(df)), collapse = ", "))
  }

  # Map clinical stage strings to ordered factor
  stage_levels <- c("Phase 0 (Exploratory)", "Phase I", "Phase II",
                     "Phase III", "Phase IV (Approved)")
  # Normalize: exact match or prefix match for flexibility
  df$stage_label <- df$maxClinicalStage
  df$stage_order <- match(df$maxClinicalStage, stage_levels)
  # Fall back to alphabetical ordering for unknown stages
  df$stage_order[is.na(df$stage_order)] <- 0L

  df <- df[order(-df$stage_order, df$disease.name), ]
  if (nrow(df) > top_n) df <- df[seq_len(top_n), ]

  # Ordered factor for fill color
  present_stages <- intersect(stage_levels, unique(df$maxClinicalStage))
  if (length(present_stages) == 0) present_stages <- unique(df$maxClinicalStage)
  df$stage_label <- factor(df$stage_label, levels = present_stages)
  df$disease.name <- factor(df$disease.name, levels = rev(df$disease.name))

  # Color palette: yellow (early) to green (approved)
  n_stages <- length(levels(df$stage_label))
  stage_colors <- grDevices::colorRampPalette(c("#FDE68A", "#16A34A"))(n_stages)

  ggplot2::ggplot(df, ggplot2::aes(x = stage_order, y = disease.name,
                                   fill = stage_label)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::scale_fill_manual(values = stats::setNames(stage_colors,
                                                        levels(df$stage_label))) +
    ggplot2::labs(
      title = "Drug Indications by Clinical Stage",
      x = "Clinical Stage", y = NULL, fill = "Stage"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 9),
                   axis.text.x = ggplot2::element_blank())
}
