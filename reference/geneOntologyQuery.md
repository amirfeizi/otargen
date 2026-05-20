# Retrieve Gene Ontology data for a specified gene.

This function queries the Open Targets GraphQL API to retrieve gene
ontology data for a specified gene.

## Usage

``` r
geneOntologyQuery(ensgId)
```

## Arguments

- ensgId:

  Character: ENSEMBL ID of the target gene (e.g., ENSG00000141510).

## Value

Returns a tibble containing gene ontology data for the specified gene.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- geneOntologyQuery(ensgId = "ENSG00000141510")
} # }
```
