## R CMD check results

0 errors | 0 warnings | 0 note

* This is a patch release (2.0.1) fixing HTTP 400 errors caused by
  upstream Open Targets Platform API schema changes. Nine query
  functions were updated to use renamed/replaced GraphQL fields:
  chemblQuery, clinVarQuery, geneBurdenQuery, orphanetQuery,
  indicationsQuery, knownDrugsChemblQuery, knownDrugsGeneQuery,
  geneOntologyQuery, and interactionsQuery.
