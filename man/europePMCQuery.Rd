% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/europePMCQuery.R
\name{europePMCQuery}
\alias{europePMCQuery}
\title{Retrieve Europe PMC data for a specified gene and disease.}
\usage{
europePMCQuery(ensemblId, efoId, cursor = NULL, size = 50)
}
\arguments{
\item{ensemblId}{Character: ENSEMBL ID of the target gene (e.g., "ENSG00000080815").}

\item{efoId}{Character: EFO ID of the target disease (e.g., "MONDO_0004975").}

\item{cursor}{Character: Cursor for pagination (default: NULL).}

\item{size}{Integer: Number of records to retrieve (default: 50).}
}
\value{
Returns a tibble containing Europe PMC evidence data for the specified gene and disease.
}
\description{
This function queries the Open Targets GraphQL API to retrieve Europe PMC evidence data
for a specified gene and disease.
}
\examples{
\dontrun{
result <- europePMCQuery(ensemblId = "ENSG00000080815",
 efoId = "MONDO_0004975", size = 50)
result <- europePMCQuery(ensemblId = "ENSG00000080815", 
efoId = "MONDO_0004975", cursor = NULL, size = 50)
}
}
