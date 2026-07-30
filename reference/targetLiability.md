# Score a target's safety liability for inhibition or knockout.

`targetLiability()` is a convenience wrapper that combines the evidence
returned by
[`geneticConstraintQuery`](https://amirfeizi.github.io/otargen/reference/geneticConstraintQuery.md),
[`safetyQuery`](https://amirfeizi.github.io/otargen/reference/safetyQuery.md)
and (optionally)
[`depMapQuery`](https://amirfeizi.github.io/otargen/reference/depMapQuery.md)
into a single, interpretable liability score with a written rationale.
It is intended as a quick triage step when prioritising drug targets:
genes that are highly constrained, essential in cell lines, or already
associated with safety events are flagged as risky for full inhibition,
while genes lacking these signals may tolerate a partial or
tissue-restricted approach.

## Usage

``` r
targetLiability(ensgId, include_essentiality = TRUE, verbose = TRUE)
```

## Arguments

- ensgId:

  Character: ENSEMBL ID of the target gene (e.g., "ENSG00000141510").

- include_essentiality:

  Logical: whether to query DepMap essentiality as part of the score.
  Set to `FALSE` to skip the extra API call (default: TRUE).

- verbose:

  Logical: print the rationale to the console (default: TRUE).

## Value

A one-row tibble with the input identifiers, the individual component
scores and flags, the combined `liability_score`, a `liability_category`
("Low", "Moderate" or "High"), a `recommendation`, and a human-readable
`rationale`. Returns `NULL` if none of the underlying queries return
data.

## Details

The overall `liability_score` is a weighted average (range 0-1, higher
means greater liability) of up to three normalised components: genetic
constraint (weight 0.40), known safety liabilities (0.35) and DepMap
essentiality (0.25). Components without data are dropped and the
remaining weights are renormalised. See *Details* for the mapping of
each component.

**Genetic constraint** uses the loss-of-function (LoF) decile bin from
[`geneticConstraintQuery()`](https://amirfeizi.github.io/otargen/reference/geneticConstraintQuery.md)
(gnomAD LOEUF, where bin 0 is the most constrained and bin 9 the least).
The most constrained bin scores 1 and the least constrained scores 0.

**Safety liabilities** count the distinct events reported by
[`safetyQuery()`](https://amirfeizi.github.io/otargen/reference/safetyQuery.md);
the component saturates at five or more events.

**Essentiality** uses the median CRISPR gene-effect score across DepMap
screens; a score of 0 (non-essential) maps to 0 and -1 (strongly
essential) maps to 1.

This is a heuristic aid, not a validated clinical safety measure; always
review the underlying evidence before acting on the score.

## Examples

``` r
if (FALSE) { # \dontrun{
# Score TP53 for inhibition liability
targetLiability(ensgId = "ENSG00000141510")

# Skip the DepMap essentiality call
targetLiability(ensgId = "ENSG00000169174", include_essentiality = FALSE)
} # }
```
