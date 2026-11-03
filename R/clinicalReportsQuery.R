#' Retrieve clinical reports (Open Targets Mira) for a drug or by report ID.
#'
#' This function queries the Open Targets GraphQL API to retrieve the harmonised
#' \emph{clinical reports} produced by the Open Targets Mira pipeline. Clinical
#' reports are the individual trial, drug-label and regulatory records that
#' underpin the Platform's drug-indication ("Clinical Precedence") evidence,
#' aggregated from sources such as ClinicalTrials.gov (via AACT), DailyMed, the
#' EMA and PMDA. See the Open Targets blog post
#' \emph{Introducing Open Targets Mira}
#' (\url{https://blog.opentargets.org/introducing-open-targets-mira/}) for
#' background.
#'
#' Provide \strong{exactly one} of \code{chemblId} or \code{reportIds}. With
#' \code{chemblId}, the function walks the drug's indications and returns every
#' distinct clinical report linked to that drug. With \code{reportIds}, it
#' fetches the named reports directly via the Platform's \code{clinicalReports}
#' root query.
#'
#' @param chemblId Character: ChEMBL ID of a drug (e.g., "CHEMBL941"). Returns
#'   all clinical reports linked to the drug's indications.
#' @param reportIds Character vector: one or more clinical report IDs
#'   (e.g., "nct00080665"). Returns those reports directly.
#'
#' @return Returns a tibble with one row per clinical report and the following
#'   columns:
#' \itemize{
#'   \item{\code{reportId}:} Clinical report identifier.
#'   \item{\code{drugId}:} The queried ChEMBL ID (\code{NA} when fetching by
#'     report ID).
#'   \item{\code{title}:} Report / trial title.
#'   \item{\code{type}:} Report type ("INDICATION" or "SAFETY").
#'   \item{\code{clinicalStage}:} Harmonised clinical stage.
#'   \item{\code{trialPhase}:} Trial phase.
#'   \item{\code{trialOverallStatus}:} Overall trial status.
#'   \item{\code{trialPrimaryPurpose}:} Primary purpose of the intervention.
#'   \item{\code{year}:} Year the report refers to.
#'   \item{\code{origin}:} Nature of the source record (e.g., CLINICAL_TRIAL).
#'   \item{\code{provider}:} Distributor of the data (e.g., AACT, EMA, PMDA).
#'   \item{\code{source}:} Primary source (e.g., ClinicalTrials.gov).
#'   \item{\code{sponsorName}:} Trial sponsor name.
#'   \item{\code{sponsorClass}:} Sponsor agency class.
#'   \item{\code{countries}:} Comma-separated countries where the report was
#'     conducted.
#'   \item{\code{diseaseIds} / \code{diseaseNames}:} Comma-separated diseases
#'     associated with the report.
#'   \item{\code{drugIds} / \code{drugNames}:} Comma-separated drugs mentioned
#'     in the report.
#'   \item{\code{url}:} Link to the source record.
#' }
#' Returns \code{NULL} if no clinical reports are found.
#'
#' @examples
#' \dontrun{
#' # All clinical reports linked to imatinib's indications
#' clinicalReportsQuery(chemblId = "CHEMBL941")
#'
#' # Fetch specific reports by ID
#' clinicalReportsQuery(reportIds = c("nct00080665", "nct00471497"))
#' }
#' @importFrom tibble tibble
#' @importFrom ghql GraphqlClient Query
#' @importFrom jsonlite fromJSON
#' @importFrom cli cli_progress_step
#' @export
#'
clinicalReportsQuery <- function(chemblId = NULL, reportIds = NULL) {
  has_chembl <- !is.null(chemblId) && length(chemblId) == 1 &&
    is.character(chemblId) && nzchar(chemblId)
  has_ids <- !is.null(reportIds) && length(reportIds) >= 1 &&
    is.character(reportIds) && all(nzchar(reportIds))

  # Require exactly one of the two inputs.
  if (has_chembl == has_ids) {
    stop("Please provide exactly one of 'chemblId' (a single ChEMBL ID) or ",
         "'reportIds' (a character vector of clinical report IDs).")
  }

  tryCatch({
    cli::cli_progress_step("Connecting to the Open Targets GraphQL API...", spinner = TRUE)
    con <- ghql::GraphqlClient$new("https://api.platform.opentargets.org/api/v4/graphql")
    qry <- ghql::Query$new()

    # Shared selection of clinical report fields.
    report_fields <- "
        id
        origin
        provider
        source
        title
        url
        clinicalStage
        trialPhase
        trialOverallStatus
        trialPrimaryPurpose
        year
        type
        trialSponsor { name agencyClass }
        diseases { diseaseFromSource disease { id name } }
        drugs { drugFromSource drug { id name } }"

    if (has_chembl) {
      query <- paste0(
        "query ClinicalReportsByDrug($chemblId: String!) {
          drug(chemblId: $chemblId) {
            id
            indications {
              rows {
                clinicalReports {", report_fields, "
                }
              }
            }
          }
        }")
      variables <- list(chemblId = chemblId)
      qry$query(name = "clinrep_by_drug", x = query)
      cli::cli_progress_step(paste0("Downloading clinical reports for ChEMBL ID: ",
                                    chemblId, " ..."), spinner = TRUE)
      # Parse as nested lists (simplifyVector = FALSE) so single- and
      # multi-row responses are handled identically, mirroring the robust
      # traversal used in gwasColocalisation().
      parsed <- jsonlite::fromJSON(
        con$exec(qry$queries$clinrep_by_drug, variables),
        simplifyVector = FALSE
      )
      rows <- parsed$data$drug$indications$rows
      # Flatten reports across all indication rows, then de-duplicate by id.
      reports <- unlist(lapply(rows, function(r) r$clinicalReports),
                        recursive = FALSE)
    } else {
      query <- paste0(
        "query ClinicalReportsByIds($reportIds: [String!]!) {
          clinicalReports(clinicalReportsIds: $reportIds) {", report_fields, "
          }
        }")
      variables <- list(reportIds = as.list(reportIds))
      qry$query(name = "clinrep_by_ids", x = query)
      cli::cli_progress_step("Downloading clinical reports ...", spinner = TRUE)
      parsed <- jsonlite::fromJSON(
        con$exec(qry$queries$clinrep_by_ids, variables),
        simplifyVector = FALSE
      )
      reports <- parsed$data$clinicalReports
    }

    if (is.null(reports) || length(reports) == 0) {
      message("No clinical reports found for the given parameters.")
      return(NULL)
    }

    # De-duplicate by report id (a report can appear under several indications).
    ids <- vapply(reports, function(r) {
      if (is.null(r$id)) NA_character_ else as.character(r$id)
    }, character(1))
    reports <- reports[!duplicated(ids) & !is.na(ids)]

    # Safe nested getter: NULL if any element of the key path is missing.
    get_in <- function(node, keys) {
      val <- node
      for (k in keys) {
        if (!is.list(val) || is.null(val[[k]])) return(NULL)
        val <- val[[k]]
      }
      val
    }
    chr <- function(keys) vapply(reports, function(r) {
      v <- get_in(r, keys)
      if (is.null(v) || length(v) == 0) NA_character_ else as.character(v)[1]
    }, character(1))
    int <- function(keys) vapply(reports, function(r) {
      v <- get_in(r, keys)
      if (is.null(v) || length(v) == 0) NA_integer_ else as.integer(v)[1]
    }, integer(1))
    # Collapse a top-level list field of scalars to a comma-separated string.
    collapse_scalar <- function(key) vapply(reports, function(r) {
      v <- r[[key]]
      if (is.null(v) || length(v) == 0) NA_character_
      else paste(unlist(v), collapse = ", ")
    }, character(1))
    # Collapse a list field of records to a comma-separated string of one key.
    collapse_rec <- function(list_key, inner_keys) vapply(reports, function(r) {
      items <- r[[list_key]]
      if (is.null(items) || length(items) == 0) return(NA_character_)
      vals <- vapply(items, function(it) {
        v <- get_in(it, inner_keys)
        if (is.null(v) || length(v) == 0) NA_character_ else as.character(v)[1]
      }, character(1))
      vals <- vals[!is.na(vals)]
      if (length(vals) == 0) NA_character_ else paste(vals, collapse = ", ")
    }, character(1))

    output <- tibble::tibble(
      reportId            = chr("id"),
      drugId              = if (has_chembl) chemblId else NA_character_,
      title               = chr("title"),
      type                = chr("type"),
      clinicalStage       = chr("clinicalStage"),
      trialPhase          = chr("trialPhase"),
      trialOverallStatus  = chr("trialOverallStatus"),
      trialPrimaryPurpose = chr("trialPrimaryPurpose"),
      year                = int("year"),
      origin              = chr("origin"),
      provider            = chr("provider"),
      source              = chr("source"),
      sponsorName         = chr(c("trialSponsor", "name")),
      sponsorClass        = chr(c("trialSponsor", "agencyClass")),
      countries           = collapse_scalar("countries"),
      diseaseIds          = collapse_rec("diseases", c("disease", "id")),
      diseaseNames        = collapse_rec("diseases", c("disease", "name")),
      drugIds             = collapse_rec("drugs", c("drug", "id")),
      drugNames           = collapse_rec("drugs", c("drug", "name")),
      url                 = chr("url")
    )

    return(output)

  }, error = function(e) {
    if (grepl("Timeout was reached", e$message)) {
      stop("Connection timeout reached while connecting to the Open Targets GraphQL API.")
    } else {
      stop(e)
    }
  })
}
