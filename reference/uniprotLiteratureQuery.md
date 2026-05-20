# Retrieve UniProt Literature data for a specified gene and disease.

This function queries the Open Targets GraphQL API to retrieve UniProt
literature evidence data for a specified gene and disease.

## Usage

``` r
uniprotLiteratureQuery(ensemblId, efoId, size = 3500)
```

## Arguments

- ensemblId:

  Character: ENSEMBL ID of the target gene (e.g., "ENSG00000130203").

- efoId:

  Character: EFO ID of the target disease (e.g., "MONDO_0004975").

- size:

  Integer: Number of records to retrieve (default: 3500).

## Value

Returns a tibble containing UniProt literature evidence data for the
specified gene and disease.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- uniprotLiteratureQuery(ensemblId = "ENSG00000130203", efoId = 
"MONDO_0004975", size = 3500)
} # }
```
