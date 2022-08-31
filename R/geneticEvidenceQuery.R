geneticEvidenceQuery <- function(efo,ensid) {
  otp_cli <- GraphqlClient$new(url = 'https://api.platform.opentargets.org/api/v4/graphql')
  otp_qry <- Query$new()

  otp_qry$query(
    'genetic_evidence_query',
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
 ')

  ## Execute the query
  variables <-
    list(efoId = "EFO_0000540",
         ensemblIds = ensid)
  result <-
    fromJSON(otp_cli$exec(otp_qry$queries$genetic_evidence_query, variables, flatten = TRUE))

  evidence <- result$data$disease$evidences$rows

  return(evidence)

}

