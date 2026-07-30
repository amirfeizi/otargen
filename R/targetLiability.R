#' Score a target's safety liability for inhibition or knockout.
#'
#' \code{targetLiability()} is a convenience wrapper that combines the
#' evidence returned by \code{\link{geneticConstraintQuery}},
#' \code{\link{safetyQuery}} and (optionally) \code{\link{depMapQuery}} into a
#' single, interpretable liability score with a written rationale. It is
#' intended as a quick triage step when prioritising drug targets: genes that
#' are highly constrained, essential in cell lines, or already associated with
#' safety events are flagged as risky for full inhibition, while genes lacking
#' these signals may tolerate a partial or tissue-restricted approach.
#'
#' The overall \code{liability_score} is a weighted average (range 0-1, higher
#' means greater liability) of up to three normalised components: genetic
#' constraint (weight 0.40), known safety liabilities (0.35) and DepMap
#' essentiality (0.25). Components without data are dropped and the remaining
#' weights are renormalised. See \emph{Details} for the mapping of each
#' component.
#'
#' @details
#' \strong{Genetic constraint} uses the loss-of-function (LoF) decile bin from
#' \code{geneticConstraintQuery()} (gnomAD LOEUF, where bin 0 is the most
#' constrained and bin 9 the least). The most constrained bin scores 1 and the
#' least constrained scores 0.
#'
#' \strong{Safety liabilities} count the distinct events reported by
#' \code{safetyQuery()}; the component saturates at five or more events.
#'
#' \strong{Essentiality} uses the median CRISPR gene-effect score across DepMap
#' screens; a score of 0 (non-essential) maps to 0 and -1 (strongly essential)
#' maps to 1.
#'
#' This is a heuristic aid, not a validated clinical safety measure; always
#' review the underlying evidence before acting on the score.
#'
#' @param ensgId Character: ENSEMBL ID of the target gene
#'   (e.g., "ENSG00000141510").
#' @param include_essentiality Logical: whether to query DepMap essentiality as
#'   part of the score. Set to \code{FALSE} to skip the extra API call
#'   (default: TRUE).
#' @param verbose Logical: print the rationale to the console (default: TRUE).
#'
#' @return A one-row tibble with the input identifiers, the individual
#'   component scores and flags, the combined \code{liability_score}, a
#'   \code{liability_category} ("Low", "Moderate" or "High"), a
#'   \code{recommendation}, and a human-readable \code{rationale}. Returns
#'   \code{NULL} if none of the underlying queries return data.
#'
#' @examples
#' \dontrun{
#' # Score TP53 for inhibition liability
#' targetLiability(ensgId = "ENSG00000141510")
#'
#' # Skip the DepMap essentiality call
#' targetLiability(ensgId = "ENSG00000169174", include_essentiality = FALSE)
#' }
#' @importFrom tibble tibble
#' @importFrom stats median
#' @importFrom dplyr bind_rows
#' @export
#'
targetLiability <- function(ensgId, include_essentiality = TRUE, verbose = TRUE) {
  if (missing(ensgId) || is.null(ensgId) || length(ensgId) != 1 ||
      !is.character(ensgId) || !nzchar(ensgId)) {
    stop("Please provide a single ENSEMBL ID string for the 'ensgId' argument.")
  }

  # ---- 1. Genetic constraint (LoF intolerance) ---------------------------
  constraint_component <- NA_real_
  constraint_loeuf     <- NA_real_
  constraint_bin       <- NA_real_
  approvedSymbol       <- NA_character_
  gc <- tryCatch(geneticConstraintQuery(ensgId), error = function(e) NULL)
  if (!is.null(gc) && nrow(gc) > 0) {
    if ("approvedSymbol" %in% names(gc)) approvedSymbol <- gc$approvedSymbol[1]
    # Only the loss-of-function row is meaningful for knockout liability;
    # if it is absent, leave the constraint component as NA rather than
    # mixing in synonymous/missense constraint types.
    lof <- gc[!is.na(gc$constraintType) & gc$constraintType == "lof", ,
              drop = FALSE]
    # Prefer the decile bin (upperBin: 0 = most constrained .. 9 = least)
    if (nrow(lof) > 0 && "upperBin" %in% names(lof) && !all(is.na(lof$upperBin))) {
      constraint_bin       <- suppressWarnings(min(lof$upperBin, na.rm = TRUE))
      constraint_component <- 1 - (constraint_bin / 9)
    } else if ("upperBin6" %in% names(lof) && !all(is.na(lof$upperBin6))) {
      constraint_bin       <- suppressWarnings(min(lof$upperBin6, na.rm = TRUE))
      constraint_component <- 1 - (constraint_bin / 5)
    }
    if ("score" %in% names(lof) && !all(is.na(lof$score))) {
      constraint_loeuf <- suppressWarnings(min(lof$score, na.rm = TRUE))
    }
    if (!is.na(constraint_component)) {
      constraint_component <- max(0, min(1, constraint_component))
    }
  }

  # ---- 2. Known safety liabilities ---------------------------------------
  n_safety_events  <- NA_integer_
  safety_component <- NA_real_
  sf <- tryCatch(safetyQuery(ensgId), error = function(e) NULL)
  if (!is.null(sf) && nrow(sf) > 0 && "event" %in% names(sf)) {
    events           <- sf$event[!is.na(sf$event)]
    n_safety_events  <- length(unique(events))
    safety_component <- min(n_safety_events / 5, 1)
  } else if (!is.null(sf)) {
    n_safety_events  <- 0L
    safety_component <- 0
  }

  # ---- 3. DepMap essentiality (optional) ---------------------------------
  essentiality_median    <- NA_real_
  essentiality_component <- NA_real_
  if (isTRUE(include_essentiality)) {
    dm <- tryCatch(depMapQuery(ensgId), error = function(e) NULL)
    if (!is.null(dm) && nrow(dm) > 0 && "screens" %in% names(dm)) {
      screens <- tryCatch(dplyr::bind_rows(dm$screens), error = function(e) NULL)
      if (!is.null(screens) && "geneEffect" %in% names(screens)) {
        eff <- suppressWarnings(as.numeric(screens$geneEffect))
        eff <- eff[is.finite(eff)]
        if (length(eff) > 0) {
          essentiality_median    <- stats::median(eff)
          essentiality_component <- max(0, min(1, -essentiality_median))
        }
      }
    }
  }

  # ---- Combine into a single score ---------------------------------------
  comps   <- c(constraint   = constraint_component,
               safety       = safety_component,
               essentiality = essentiality_component)
  weights <- c(constraint   = 0.40,
               safety       = 0.35,
               essentiality = 0.25)
  keep <- !is.na(comps)
  if (!any(keep)) {
    message("No genetic constraint, safety or essentiality data found for ", ensgId, ".")
    return(NULL)
  }
  liability_score <- round(sum(comps[keep] * weights[keep]) / sum(weights[keep]), 3)

  liability_category <- if (liability_score >= 0.66) {
    "High"
  } else if (liability_score >= 0.34) {
    "Moderate"
  } else {
    "Low"
  }

  # ---- Interpretable flags -----------------------------------------------
  constraint_flag <- !is.na(constraint_component) && constraint_component >= 0.8
  safety_flag     <- !is.na(n_safety_events) && n_safety_events > 0
  essential_flag  <- !is.na(essentiality_median) && essentiality_median <= -0.5

  # ---- Rationale ---------------------------------------------------------
  bits <- character(0)
  if (!is.na(constraint_component)) {
    bits <- c(bits, sprintf(
      "genetic constraint %s (LoF bin %s%s)",
      if (constraint_flag) "HIGH" else "moderate/low",
      ifelse(is.na(constraint_bin), "NA", constraint_bin),
      ifelse(is.na(constraint_loeuf), "", sprintf(", LOEUF %.2f", constraint_loeuf))
    ))
  } else {
    bits <- c(bits, "no genetic-constraint data")
  }
  if (!is.na(n_safety_events)) {
    bits <- c(bits, sprintf("%d known safety liabilit%s", n_safety_events,
                            ifelse(n_safety_events == 1, "y", "ies")))
  } else {
    bits <- c(bits, "no safety data")
  }
  if (isTRUE(include_essentiality)) {
    if (!is.na(essentiality_median)) {
      bits <- c(bits, sprintf("DepMap median gene effect %.2f (%s)",
                              essentiality_median,
                              if (essential_flag) "essential" else "tolerated"))
    } else {
      bits <- c(bits, "no essentiality data")
    }
  }

  recommendation <- if (liability_category == "High") {
    paste0("High liability: full inhibition or knockout looks risky. ",
           "Consider partial inhibition or a tissue-restricted approach.")
  } else if (liability_category == "Moderate") {
    paste0("Moderate liability: proceed with caution and review the ",
           "individual safety, constraint and essentiality evidence.")
  } else {
    paste0("Low liability: available evidence suggests the target may ",
           "tolerate full inhibition.")
  }

  rationale <- sprintf("%s (score %.2f). %s Signals: %s.",
                       liability_category, liability_score,
                       recommendation, paste(bits, collapse = "; "))

  if (isTRUE(verbose)) {
    message(rationale)
  }

  tibble::tibble(
    geneId              = ensgId,
    approvedSymbol      = approvedSymbol,
    constraint_score    = round(constraint_component, 3),
    constraint_bin      = constraint_bin,
    constraint_loeuf    = constraint_loeuf,
    constraint_flag     = constraint_flag,
    n_safety_events     = n_safety_events,
    safety_score        = round(safety_component, 3),
    safety_flag         = safety_flag,
    essentiality_median = round(essentiality_median, 3),
    essentiality_score  = round(essentiality_component, 3),
    essential_flag      = essential_flag,
    liability_score     = liability_score,
    liability_category  = liability_category,
    recommendation      = recommendation,
    rationale           = rationale
  )
}
