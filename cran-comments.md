## R CMD check results

0 errors | 0 warnings | 1 note

* **NOTE**: `checking for future file timestamps ... unable to verify current time`
  — this is a transient network issue during the check, not a package problem.

## Test environments

* Windows 11, R 4.x (local via RStudio)

## Submission notes

* This is a minor release (2.2.0) adding one new feature,
  `clinicalReportsQuery()`, which retrieves the harmonised clinical reports
  from the Open Targets "Mira" pipeline via the Platform GraphQL API (by drug
  ChEMBL ID or by report ID) and returns them as a tidy tibble. No changes to
  existing function interfaces.

* This version also fixes the invalid (404) URL in README.md that was flagged
  in the previous 2.1.0 submission: the GitHub-stars badge linked to
  `https://github.com/amirfeizi/otargen/stargazers` and now links to the
  repository home page, `https://github.com/amirfeizi/otargen`, which resolves
  correctly.

* The 2.1.0 feature `targetLiability()` (a wrapper over
  `geneticConstraintQuery()`, `safetyQuery()` and `depMapQuery()`) is also
  included, as 2.1.0 was not accepted on CRAN.
