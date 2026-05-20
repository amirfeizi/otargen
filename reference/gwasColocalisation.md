# Retrieve calculated GWAS colocalisation data

This function retrieves colocalisation data for a specific study locus
from a GWAS study with other GWAS studies. It returns a data frame of
the studies that colocalise with the input study locus, including
details on the study, reported trait, index variant, and calculated
colocalisation method outputs.

## Usage

``` r
gwasColocalisation(study_locus_id, size = 500, index = 0)
```

## Arguments

- study_locus_id:

  Character: Open Target Genetics generated ID for the study locus
  (e.g., "5a86bfd40d2ebecf6ce97bbe8a737512").

- size:

  Integer: Number of rows to fetch per page. Default: 500.

- index:

  Integer: Page index for pagination. Default: 0.

## Value

Returns a data frame of the studies that colocalise with the input study
locus. The table consists of the following data structure:

- `study.studyId`: *Character vector*. Study identifier.

- `study.traitReported`: *Character vector*. Reported trait associated
  with the colocalisation.

- `study.projectId`: *Character vector*. Project identifier for the
  study.

- `study.publicationFirstAuthor`: *Character vector*. First author of
  the publication.

- `indexVariant.id`: *Character vector*. Index variant identifier.

- `indexVariant.position`: *Integer vector*. Index variant position.

- `indexVariant.chromosome`: *Character vector*. Index variant
  chromosome.

- `indexVariant.referenceAllele`: *Character vector*. Reference allele
  of the variant.

- `indexVariant.alternateAllele`: *Character vector*. Alternate allele
  of the variant.

- `pValueMantissa`: *Numeric vector*. Mantissa of the p-value for the
  colocalisation.

- `pValueExponent`: *Integer vector*. Exponent of the p-value for the
  colocalisation.

- `numberColocalisingVariants`: *Integer vector*. Number of colocalising
  variants.

- `colocalisationMethod`: *Character vector*. Method used for
  colocalisation analysis.

- `h3`: *Numeric vector*. H3 value associated with the colocalisation.

- `h4`: *Numeric vector*. H4 value associated with the colocalisation.

- `clpp`: *Numeric vector*. Colocalisation posterior probability.

- `betaRatioSignAverage`: *Numeric vector*. Average sign of the beta
  ratio.

## References

Giambartolomei, Claudia et al. “Bayesian test for colocalisation between
pairs of genetic association studies using summary statistics.” PLoS
genetics vol. 10,5 e1004383. 15 May. 2014,
doi:10.1371/journal.pgen.1004383

## Examples

``` r
if (FALSE) { # \dontrun{
colocalisation_data <- gwasColocalisation(study_locus_id = "5a86bfd40d2ebecf6ce97bbe8a737512",
 size = 500, index = 0)
} # }
```
