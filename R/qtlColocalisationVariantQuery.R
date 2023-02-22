#' Retrieves colocalization data for a variant in a study
#'
#' For an input study id and variant id, a table is generated as given in the example.
#'
#' @param studyid String: Open Target Genetics generated id for gwas studies.
#' @param variantid String: Open Target Genetics generated id for variant (CHR_POSITION_REFALLELE_ALT_ALLELE or rsId).
#'
#' @return A data frame of the colocalization information for a lead variant.
#' @examples
#' qtl_coloc <- qtlColocalisationVariantQuery(studyid="GCST90002357",variantid="1_154119580_C_A")
#' or
#' qtl_coloc <- qtlColocalisationVariantQuery(studyid="GCST90002357",variantid="rs2494663")
#'
#' qtl_coloc
#'    qtlStudyName                                      phenotypeId         gene.id     gene.symbol                          name indexVariant.id indexVariant.rsId         beta        h4         h3 log2h4h3
#'     GTEx-sQTL   chr1^154570367^154571189^clu_38024^ENSG00000160716 ENSG00000160716      CHRNB2               Brain hippocampus 1_153673124_G_A       rs150151810  0.180563301 0.1058276 0.06300702 0.748131
#'       eQTLGen                                      ENSG00000163221 ENSG00000163221     S100A12                           Blood 1_153705169_T_G       rs115182992 -0.007249468 0.8150887 0.17168439 2.247198
#'     GTEx-sQTL   chr1^153759613^153760311^clu_47736^ENSG00000143624 ENSG00000143624       INTS3          Heart atrial appendage 1_153744649_G_A        rs78105201  0.208291218 0.5487299 0.04672203 3.553921
#'
#' @export
#'
#'

qtlColocalisationVariantQuery <- function(studyid, variantid) {

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


  ## Query for QTL colocalisation

  query <- "query qtlColocalisationVariantQuery($studyId: String!, $variantId: String!) {
  qtlColocalisation(studyId: $studyId, variantId: $variantId){
    qtlStudyName
    phenotypeId
    gene {
      id
      symbol
    }
    tissue {
      name
    }
    indexVariant {
      id
      rsId
    }
    beta
    h4
    h3
    log2h4h3
  }
}"

  # execute the query

  variables <- list(studyId = studyid, variantId = input_variantid)


  otg_qry$query(name = "qtl_query", x = query)

  cli::cli_progress_step("Downloading data...", spinner = TRUE)
  qtlcoloc_result <- jsonlite::fromJSON(otg_cli$exec(otg_qry$queries$qtl_query, variables, flatten = TRUE))$data
  df_qtlcoloc <- as.data.frame(qtlcoloc_result$qtlColocalisation)
  return(df_qtlcoloc)
}
