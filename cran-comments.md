## R CMD check results

0 errors | 0 warnings | 1 note

* **NOTE**: `checking for future file timestamps ... unable to verify current time`
  — this is a transient network issue during the check, not a package problem.

## Test environments

* Windows 11, R 4.x (local via RStudio)

## Submission notes

* This is a patch release (2.0.1) fixing HTTP 400 errors caused by
  upstream Open Targets Platform API schema changes. Nine query
  functions were updated to use renamed/replaced GraphQL fields:
  chemblQuery, clinVarQuery, geneBurdenQuery, orphanetQuery,
  indicationsQuery, knownDrugsChemblQuery, knownDrugsGeneQuery,
  geneOntologyQuery, and interactionsQuery.
