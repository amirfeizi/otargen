
associatedTargets <- function(efoid,n) {

  otp_cli <- GraphqlClient$new(url = 'https://api.platform.opentargets.org/api/v4/graphql')
  otp_qry <- Query$new()

  ## Query for targets associated with a disease
  otp_qry$query('simple_query', 'query simpleQuery($efoId: String!){
  disease(efoId: $efoId){
    name
    associatedTargets{
      rows{
        target{
          id
          approvedName
        }
        datatypeScores{
          id
          score
        }
      }
    }
  }
}'
  )

  ## Execute the query
  variables <- list(efoId = efoid)
  result <- fromJSON(otp_cli$exec(otp_qry$queries$simple_query, variables, flatten = TRUE))$data$disease


  score_dt <- lapply(result$associatedTargets$rows$datatypeScores, spread, key = id, value = score)


  score_dt_wd <- rbindlist(score_dt, fill = TRUE)


  results_final <- cbind(result$associatedTargets$rows$target, score_dt_wd)
  return(results_final[1:n,])

}
