% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/overlapInfoForStudy.R
\name{overlapInfoForStudy}
\alias{overlapInfoForStudy}
\title{Retrieves overlap info for a study and a list of studies}
\usage{
overlapInfoForStudy(study_id, study_ids = list())
}
\arguments{
\item{study_id}{Character: Study ID(s) generated by Open Targets (e.g GCST90002357).}

\item{study_ids}{Character: generated ID for variants by Open Targets (e.g. 1_154119580_C_A) or rsId (rs2494663).}
}
\value{
A list containing a data frame of overlap information and the variant intersection set.
The overlap information table (overlap_info) consists of the following columns:
\itemize{
  \item{\code{studyId}:} \emph{Character vector}. Study ID.
  \item{\code{traitReported}:} \emph{Character vector}. Reported trait.
  \item{\code{traitCategory}:} \emph{Character vector}. Trait category.
  \item{\code{variantIdA}:} \emph{Character vector}. Variant ID from study A.
  \item{\code{variantIdB}:} \emph{Character vector}. Variant ID from study B.
  \item{\code{overlapAB}:} \emph{Integer vector}. Number of overlaps between variants A and B.
  \item{\code{distinctA}:} \emph{Integer vector}. Number of distinct variants in study A.
  \item{\code{distinctB}:} \emph{Integer vector}. Number of distinct variants in study B.
  \item{\code{study.studyId}:} \emph{Character vector}. Study ID from study list.
  \item{\code{study.traitReported}:} \emph{Character vector}. Reported trait from study list.
  \item{\code{study.traitCategory}:} \emph{Character vector}. Trait category from study list.
}
The variant intersection set (variant_intersection_set) is a character vector representing the intersection of variants.
}
\description{
For an input study ID and a list of other study IDs, this function returns two elements.
One contains the overlap information in a table format, and the other element is the variant intersection set,
representing an overlap between two variants of the two given studies.
}
\examples{
\dontrun{
result <- overlapInfoForStudy(study_id = "GCST90002357",
 study_ids = list("GCST90025975", "GCST90025962"))
}
}
