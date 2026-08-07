#' GWAS Credible Sets for a Variant
#'
#' Queries the Open Targets Platform API to retrieve GWAS credible sets
#' associated with a specific variant, including study details,
#' disease/trait information, and locus-to-gene predictions.
#'
#' @param variantId A string specifying the variant ID (e.g., "1_66008238_T_C").
#' @param size Integer specifying the number of results to return per page (default: 500).
#' @param index Integer specifying the page index for pagination (default: 0).
#' @return A tibble containing GWAS credible set data for the specified variant.
#' @export
#' @examples
#' \dontrun{
#' gwasCredibleSetsByVariant(variantId = "1_66008238_T_C")
#' }
gwasCredibleSetsByVariant <- function(variantId, size = 500L, index = 0L) {
  con <- ghql::GraphqlClient$new(url = "https://api.platform.opentargets.org/api/v4/graphql")
  qry <- ghql::Query$new()
  qry$query(
    "GWASCredibleSetsByVariant",
    'query GWASCredibleSetsByVariant($variantId: String!, $size: Int!, $index: Int!) {
      variant(variantId: $variantId) {
        id
        referenceAllele
        alternateAllele
        gwasCredibleSets: credibleSets(studyTypes: [gwas], page: { size: $size, index: $index }) {
          count
          rows {
            studyLocusId
            pValueMantissa
            pValueExponent
            beta
            finemappingMethod
            confidence
            variant { id chromosome position referenceAllele alternateAllele }
            study {
              traitFromSource
              id
              diseases { name id therapeuticAreas { name id } }
            }
            locus(variantIds: [$variantId]) { rows { posteriorProbability } }
            locusSize: locus { count }
            l2GPredictions(page: { size: 1, index: 0 }) {
              rows { target { id approvedSymbol } score }
            }
          }
        }
      }
    }'
  )

  res_json <- con$exec(
    qry$queries$GWASCredibleSetsByVariant,
    variables = list(variantId = variantId, size = size, index = index)
  )
  res <- jsonlite::fromJSON(res_json, simplifyVector = TRUE)
  rows <- res$data$variant$gwasCredibleSets$rows

  if (is.null(rows) || length(rows) == 0) return(tibble::tibble())
  out <- tibble::as_tibble(rows)
  out$pValue <- format(out$pValueMantissa * 10^out$pValueExponent, scientific = TRUE)
  out <- out[ , !(names(out) %in% c("pValueMantissa", "pValueExponent")), drop = FALSE]
  out
}
