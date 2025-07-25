% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chemblQuery.R
\name{chemblQuery}
\alias{chemblQuery}
\title{Retrieve ChEMBL data for a specified gene and disease.}
\usage{
chemblQuery(ensemblId, efoId, cursor = NULL, size = 10)
}
\arguments{
\item{ensemblId}{Character: ENSEMBL ID of the target gene (e.g., "ENSG00000080815").}

\item{efoId}{Character: EFO ID of the target disease (e.g., "MONDO_0004975").}

\item{cursor}{Character: Cursor for pagination (default: NULL).}

\item{size}{Integer: Number of records to retrieve (default: 10).}
}
\value{
Returns a tibble containing ChEMBL evidence data for the specified gene and disease.
}
\description{
This function queries the Open Targets GraphQL API to retrieve ChEMBL evidence data
for a specified gene and disease.
}
\examples{
\dontrun{
result <- chemblQuery(ensemblId = "ENSG00000080815", efoId = 
"MONDO_0004975",
 size = 10)
result <- chemblQuery(ensemblId = "ENSG00000080815", efoId = 
"MONDO_0004975",
 cursor = NULL, size = 10)
}
}
