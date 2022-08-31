studyLocus2GeneTable <- function(studyid,variantid){


  ## Set up to query Open Targets Genetics API
  otg_cli <- GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- Query$new()

  ## Query for GWAS study locus details
  otg_qry$query('l2g_query', 'query l2gQuery($studyId: String!, $variantId: String!){
    studyInfo(studyId: $studyId){
    numAssocLoci
    ancestryInitial
    nTotal
    nCases
    pubAuthor
  }
  studyLocus2GeneTable(studyId: $studyId, variantId: $variantId){
    rows {
      gene {
        id
        symbol
      }
      hasColoc
      yProbaModel
      distanceToLocus
    }
  }
}')

  ## Execute the query
  variables <- list(studyId = studyid, variantId = variantid)
  result <- fromJSON(otg_cli$exec(otg_qry$queries$l2g_query, variables, flatten = TRUE))$data

  return(result)


}
