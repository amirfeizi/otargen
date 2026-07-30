## R CMD check results

0 errors | 0 warnings | 1 note

* **NOTE**: `checking for future file timestamps ... unable to verify current time`
  — this is a transient network issue during the check, not a package problem.

## Test environments

* Windows 11, R 4.x (local via RStudio)

## Submission notes

* This is a minor release (2.1.0) adding one new feature,
  `targetLiability()`: a convenience wrapper that combines the existing
  `geneticConstraintQuery()`, `safetyQuery()` and `depMapQuery()` functions
  into a single target-liability score with a written rationale. No changes
  to existing function interfaces. `stats` was added to Imports for
  `stats::median()`.
