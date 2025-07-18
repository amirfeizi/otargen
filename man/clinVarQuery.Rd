% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clinvarQuery.R
\name{clinVarQuery}
\alias{clinVarQuery}
\title{Retrieve ClinVar data for a specified gene and disease.}
\usage{
clinVarQuery(ensemblId, efoId, cursor = NULL, size = 10)
}
\arguments{
\item{ensemblId}{Character: ENSEMBL ID of the target gene (e.g., "ENSG00000080815").}

\item{efoId}{Character: EFO ID of the target disease (e.g., "MONDO_0004975").}

\item{cursor}{Character: Cursor for pagination (default: NULL).}

\item{size}{Integer: Number of records to retrieve (default: 10).}
}
\value{
Returns a tibble containing ClinVar evidence data for the specified gene and disease.
}
\description{
This function queries the Open Targets GraphQL API to retrieve ClinVar evidence data
for a specified gene and disease.
}
\examples{
\dontrun{
result <- clinVarQuery(ensemblId = "ENSG00000080815", efoId = 
"MONDO_0004975", size = 10)
result <- clinVarQuery(ensemblId = "ENSG00000080815", efoId = 
"MONDO_0004975", cursor = NULL, size = 10)
}
}
