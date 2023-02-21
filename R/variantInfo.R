#' Retrieves the information about the input variant id.
#'
#'
#' @param variantid String : Open Target Genetics generated variant id or rs id.
#'
#' @return Data frame containing the variant information.
#'
#' @examples
#' var_info <- variantInfo(variantid = "1_55039974_G_T")
#' var_info <- variantInfo(variantid = "rs11591147")
#' var_info
#' chromosome	position	refAllele	altAllele	  rsId	     chromosomeB37	positionB37	    id	         nearestGene.id	  nearestGene.symbol	nearestGeneDistance	nearestCodingGene.id	nearestCodingGene.symbol	nearestCodingGeneDistance	mostSevereConsequence	caddRaw	caddPhred	gnomadAFR	  gnomadAMR	  gnomadASJ	  gnomadEAS	 gnomadFIN	  gnomadNFE	  gnomadNFEEST	gnomadNFENWE	gnomadNFESEU	gnomadNFEONF	gnomadOTH
#'   <chr>     <int>     <chr>     <chr>     <chr>         <chr>         <int>         <chr>             <chr>             <chr>               <int>               <chr>                    <chr>                   <int>                      <chr>           <num>    <num>    <num>       <num>         <num>     <int>       <num>         <num>       <num>         <num>          <int>        <num>         <num>
#'     1	    55039974	   G	       T	    rs11591147	    1	          55505647	  1_55039974_G_T	 ENSG00000169174	     PCSK9	              527	            ENSG00000169174	            PCSK9	                  527	                  missense_variant	  1.40806	  15.41	  0.002870264	0.004716981	0.006896552	   0	     0.04689298	  0.014970836	0.014590592	   0.015941354	      0	      0.012628625	  0.014732965
#'
#' @export
#'

variantInfo <- function(variantid) {

  ## Set up to query Open Targets Genetics API


  cli::cli_progress_step("Connecting the database...", spinner = TRUE)
  otg_cli <- ghql::GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- ghql::Query$new()

  # Check variant id format
  if (grepl(pattern = "rs\\d+", variantid)) {

    # Convert rs id to variant id
    query_searchid <- "query ConvertRSIDtoVID($queryString:String!) {
    search(queryString:$queryString){
      totalVariants
      variants{
        id
        }
      }
    }"

    variables <- list(queryString = variantid)
    otg_qry$query(name = "convertid", x = query_searchid)
    id_result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$convertid, variables), flatten=TRUE)$data
    input_variantid <- id_result$search$variants$id
  }

  else if (grepl(pattern = "\\d+_\\d+_[a-zA-Z]+_[a-zA-Z]+", variantid))
  {
    input_variantid <- variantid
  }
  else
  {
    stop("\n Please provide a variant Id")
  }


  query <- "query variantInfoquery($variantId: String!){
  variantInfo(variantId: $variantId){
    chromosome
    position
    refAllele
    altAllele
    rsId
    chromosomeB37
    positionB37
    id
    nearestGene{
      id
      symbol
    }
    nearestGeneDistance
    nearestCodingGene{
      id
      symbol
    }
    nearestCodingGeneDistance
    mostSevereConsequence
    caddRaw
    caddPhred
    gnomadAFR
    gnomadAMR
    gnomadASJ
    gnomadEAS
    gnomadFIN
    gnomadNFE
    gnomadNFEEST
    gnomadNFENWE
    gnomadNFESEU
    gnomadNFEONF
    gnomadOTH
  }
  }"


  ## Execute the query

  variables <- list(variantId = input_variantid)

  otg_qry$query(name = "variantInfoquery", x =  query)

  cli::cli_progress_step("Downloading data...", spinner = TRUE)
  result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$variantInfoquery, variables), flatten = TRUE)$data
  result <- as.data.frame(result$variantInfo)
  return (result)
}
