#' Query genetic evidance for a gene in a disease
#'
#' @param efo is experimental factor ontolody id
#' @param ensid is a identification id for genes by ensembl database
#' @return a dataframe including the queried gene indentity and its colocalization data
#' @export

geneticEvidenceQuery <- function(efo, ensid) {
  otp_cli <- ghql::GraphqlClient$new(url = "https://api.platform.opentargets.org/api/v4/graphql")
  otp_qry <- ghql::Query$new()

  otp_qry$query(
    "genetic_evidence_query",
    'query geneticEvidenceQuery($efoId: String!, $ensemblIds: [String!]!){
   disease(efoId: $efoId){
     evidences(ensemblIds: $ensemblIds,
     datasourceIds: ["ot_genetics_portal"])
     {
       rows{
         target{
           id
           approvedSymbol
         }


         disease{
           id
           name
         }
         score
         diseaseFromSource
         studyId
         publicationYear
         oddsRatio
         beta
         variantId
         variantRsId
         variantFunctionalConsequence{
           label
         }
       }


     }
   }
 }
 '
  )

  ## Execute the query
  variables <-
    list(
      efoId = "EFO_0000540",
      ensemblIds = ensid
    )
  result <- jsonlite::fromJSON(otp_cli$exec(otp_qry$queries$genetic_evidence_query, variables, flatten = TRUE))

  evidence <- result$data$disease$evidences$rows

  return(evidence)
}
