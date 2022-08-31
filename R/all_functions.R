##  These are various GraphQL queries wrapped in R functions
##  to fetch data.tables from Open Target Genetics GraphQL end point
##  1- get_geninfo()
##  2- get_gene_colocal_info()
##  3- get_studiesAndLeadl2g()
##  4- get_genetic_evidance_optg() 
##  5- get_gwas_loci_l2g_optg()
##  6- get_gwas_loci_credset_optg()
##  7- get_qtl_optg()
##  8- get_top_target_disease_opt()
##  9- get_litreture_drug()

library(rjson)
library(jsonlite)
library(magrittr)
library(ghql)
library(data.table)
library(jsonlite)
library(conflicted)


conflict_prefer("flatten", "purrr")
conflict_prefer("filter", "dplyr")
conflict_prefer("fromJSON", "jsonlite")


## 1- get_geninfo()
get_geninfo <- function(ensmbl_ids){
  
  
  res2 <- data.frame()
  res_gene_info <- data.frame()
  for (input_gene in ensmbl_ids) {
    base::print(input_gene)
    qry <- Query$new()
    qry$query('getgeninfo', 'query	geneandstudy($gene:String!) {
   geneInfo (geneId:$gene) {
     id
     symbol
     description
     chromosome
     start
     end

   }
   studiesAndLeadVariantsForGene(geneId:$gene){
     study {
       pmid
       pubDate
       pubJournal
       pubTitle
       pubAuthor
       hasSumstats
       nInitial
       nReplication
       nCases
       traitCategory
       numAssocLoci
     }

   }
 }')
    
    
    variables  <- list(gene = input_gene)
    con <- GraphqlClient$new('https://api.genetics.opentargets.org/graphql')
    res <- con$exec(qry$queries$getgeninfo, variables)
    
    res1 <- jsonlite::fromJSON(res, flatten = TRUE)
    
    res1$data$studiesAndLeadVariantsForGene$gene_symbol <- rep(res1$data$geneInfo$symbol, length(res1$data$studiesAndLeadVariantsForGene$study.pmid))
    res2 <- rbind(res2, res1$data$studiesAndLeadVariantsForGene)
    res_gene_info <- rbind(res_gene_info, as.data.frame(res1$data$geneInfo))
    Sys.sleep(1)
    
    
  }
  return(res2)
  
} 


# build query for colocalisationsForGene
## 2-get_gene_colocal_info()


get_gene_colocal_info <- function(ensmbl_ids){
  
  ensmbl_ids <- ensmbl_ids
  colocal2 <- data.frame()
  colocal_genes_info <- data.frame()
  
  
  
  for (input_gene in ensmbl_ids) {
    print(input_gene)
    qry <- Query$new()
    qry$query('getgenColocal', 'query	geneandcolocal($gene:String!) {
  geneInfo (geneId:$gene) {
    id
    symbol
    description
    chromosome
    start
    end
    
  }
  
  
colocalisationsForGene(geneId:$gene){
  study {
    pmid
    pubDate
    pubJournal
    pubTitle
    pubAuthor
    hasSumstats
    nInitial
    nReplication
    nCases
    traitCategory
    numAssocLoci
  }
  tissue {
    id
  }
  phenotypeId
  log2h4h3
  qtlStudyId
  h3
  h4
}
  
}')
    
    variables  <- list(gene = input_gene)
    con <- GraphqlClient$new('https://api.genetics.opentargets.org/graphql')
    
    colocal <- con$exec(qry$queries$getgenColocal, variables)
    colocal1 <- jsonlite::fromJSON(colocal, flatten = TRUE)
    
    colocal_genes_info <- rbind(colocal_genes_info, as.data.frame(colocal1$data$geneInfo))
    
    colocal1$data$colocalisationsForGene$gene_symbol <- rep(colocal1$data$geneInfo$symbol,
                                                            length(colocal1$data$colocalisationsForGene$phenotypeId))
    colocal2 <- rbind(colocal2, colocal1$data$colocalisationsForGene)
    
    
   # Sys.sleep(1)
    
  }
return(colocal2)

}

## 3.get_studiesAndLeadl2g  
## L2G pipeline data collection 

get_studiesAndLeadl2g <- function(ensmbl_ids){

       ensmbl_ids <- ensmbl_ids
       l2g2 <- data.frame()
       l2g_genes_info <- data.frame()
       con <- GraphqlClient$new('https://api.genetics.opentargets.org/graphql')  #make a graphql connection

       for (input_gene in ensmbl_ids ) {

         base::print(input_gene)

        # Set up to query Open Targets Platform API
         qry <- Query$new()

        # Query for targets associated with a disease and L2G scores

         qry$query('getStudiesLeadL2G', 'query	studiesAndLeadl2g($gene:String!) {

             geneInfo (geneId:$gene) {
                                     id
                                     symbol
                                     description
                                     chromosome
                                     start
                                     end
                                     }
             studiesAndLeadVariantsForGeneByL2G (geneId:$gene){
                                     yProbaModel
                                     yProbaDistance
                                     yProbaInteraction
                                     yProbaMolecularQTL
                                     yProbaPathogenicity
                                     pval
                               beta {

                                     direction
                                     betaCI
                                     betaCILower
                                     betaCIUpper
                                   }
                               odds{
                                   oddsCI
                                   oddsCILower
                                   oddsCIUpper

                                   }
                               study{
                                 studyId
                                 traitReported
                                 traitCategory
                                 pubDate
                                 pubTitle
                                 pubAuthor
                                 pubJournal
                                 pmid
                                 hasSumstats
                                 nCases
                                 numAssocLoci
                                 nTotal
                                 traitEfos

                                     }
                               variant{
                                 id
                                 rsId
                                 chromosome
                                 position
                                 refAllele
                                 altAllele
                               nearestGene {
                                 id
                                           }
                                nearestCodingGene {
                                 id
                                 }
                               nearestCodingGeneDistance
                               nearestGeneDistance
                               mostSevereConsequence

     }
   }

 }')
         
## Execute the query
    
    variables  <- list(gene = input_gene) # define the input gene name
    
    l2g <- con$exec(qry$queries$getStudiesLeadL2G, variables)# execute the query
    
    l2g1 <- jsonlite::fromJSON(l2g, flatten = TRUE) # convert the query output from json
    
    
    l2g1$data$studiesAndLeadVariantsForGeneByL2G$gene_symbol <- rep(l2g1$data$geneInfo$symbol,length(l2g1$data$studiesAndLeadVariantsForGeneByL2G$yProbaModel)) 
    
    l2g2 <- bind_rows(l2g2, l2g1$data$studiesAndLeadVariantsForGeneByL2G)
    gene_info <- l2g1$data$geneInfo
    l2g_genes_info <- bind_rows(l2g_genes_info, gene_info)
    
    #Sys.sleep(1)
    
  }

return(l2g2)

}




## Query for genetic evidence from open target
## 4- get_genetic_evidance_optg() 

 get_genetic_evidance_optg <- function(efo,ensid) {
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


#interesting_evidence <- test[2,] 
#as.list(interesting_evidence) 




## 5- get_gwas_loci_l2g_optg()
get_gwas_loci_l2g_optg <- function(studyid,variantid){
  
  
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

## 6- get_gwas_loci_credset_optg()
get_gwas_loci_credset_optg <- function(studyid,variantid){ 
  
  
  ## Query for GWAS study locus details
  
  otg_cli <- GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- Query$new()
  otg_qry$query('credset_query', 'query credsetQuery($studyId: String!, $variantId: String!){
  gwasCredibleSet(studyId: $studyId, variantId: $variantId) {
    tagVariant {
      id
    }
    beta
    postProb
    pval
  }
}')
  
  ## Execute the query 
  variables <- list(studyId = studyid, variantId = variantid)
  result <- fromJSON(otg_cli$exec(otg_qry$queries$credset_query, variables, flatten = TRUE))$data
  
  result <- result$gwasCredibleSet %>% flatten()
  return(result)
  
  
}

## 7- get_qtl_optg()
get_qtl_optg <- function(studyid, variantid) {
  
  
  ## Query for QTL colocalisation
  
  otg_cli <- GraphqlClient$new(url = "https://api.genetics.opentargets.org/graphql")
  otg_qry <- Query$new()
  otg_qry$query(
    'qtl_query',
    'query qtlColocalisationVariantQuery($studyId: String!, $variantId: String!) {
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
    }
    beta
    h4
  }
}'
  )
  
  variables = list(studyId = studyid, variantId = variantid)
  result <-
    fromJSON(otg_cli$exec(otg_qry$queries$qtl_query, variables, flatten = TRUE))$data
  l2g_result <- result$qtlColocalisation
  return(l2g_result)
  
}



## 8-get_top_target_disease_opt()

get_top_target_disease_opt <- function(efoid,n) {
  
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
####################
## 9- get_litreture_drug()


get_litreture_drug <- function(efoid){
  
  otp_cli <- GraphqlClient$new(url = 'https://api.platform.opentargets.org/api/v4/graphql')
  otp_qry <- Query$new()
  
  ## Query for targets associated with a disease
  otp_qry$query('litreture_query', 'query literatureDrugQuery($efoId: String!){
  disease(efoId: $efoId) {
    name
    associatedTargets(
      aggregationFilters: [{ name: "dataTypes", path: "literature" }]
    ) {
      rows {
        score
        target {
          id
          approvedSymbol
          evidences (
            efoIds: [$efoId]
            datasourceIds: ["europepmc"]
          ){
            count
            rows {
              textMiningSentences {
                text
              }
            }
          }
          knownDrugs {
            uniqueDrugs
            rows {
              drug {
                id
                name
                maximumClinicalTrialPhase
              }
            }
          }
        }
      }
    }
  }
}'
  )
  
  
  ## Execute the query
  variables <- list(efoId = "EFO_0000540")
  result <- fromJSON(otp_cli$exec(otp_qry$queries$litreture_query, variables, flatten = TRUE))$data$disease
  
  result_targets <- result$associatedTargets$rows$target[,1:2]
  result_targets$score <- result$associatedTargets$rows$score
  
  result_targets$text_mining <- result$associatedTargets$rows$target$evidences$rows 
  
  result_targets <- result_targets %>% unnest(cols = c(text_mining)) %>%
    unnest(cols = c(textMiningSentences))
  
  result_targets <- distinct(result_targets)

  result_drugs <- result$associatedTargets$rows$target[,1:2]
  
  result_drugs$drugs <- result$associatedTargets$rows$target$knownDrugs$rows
  result_drugs <- result_drugs %>% unnest(cols = c(drugs))

  return(result_targets, result_drugs)
  
}

