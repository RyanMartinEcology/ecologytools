# Weighted Diet Quality of RRA Samples

Computes per-sample digestible protein (DP) and digestible energy (DE)
for relative-read-abundance (RRA) diet metabarcoding samples, weighting
each taxon's stage-specific forage quality by its RRA. For each sample,
every taxon is resolved to a taxonomic level via `lookup`, assigned a
phenological stage from `mode` for the sample's seasonal period, and
given a DP/DE value from `qual` for that stage. The sample score is the
RRA-weighted mean over the taxa that yield a usable value, computed
independently for DP and DE.

## Usage

``` r
calc_diet_quality(
  rra,
  qual,
  mode,
  lookup,
  meta_cols = c("sample_id", "animal_id", "date", "year", "UTME", "UTMN"),
  tol = 1e-06
)
```

## Arguments

- rra:

  A data frame of RRA samples: the columns named in `meta_cols` plus one
  numeric column per taxon, each row summing to 1.

- qual:

  A list of quality tibbles by level (`species`, `genus`, `family`,
  `functional_group`, `growth_form`), each keyed by its level's name
  column plus `Phenology` and `Part`, with `DP_mean` and `DE_mean`.

- mode:

  A list of phenology summaries by level (the five above plus `all`), as
  returned by `calc_phenology_mode`: a key column then one modal-code
  column per seasonal period.

- lookup:

  A taxonomy crosswalk with columns `species`, `genus`, `family`,
  `order`, `functional_group`, `growth_form`.

- meta_cols:

  Character vector of non-taxon columns in `rra`. Defaults to
  `c('sample_id', 'animal_id', 'date', 'year', 'UTME', 'UTMN')`.

- tol:

  Numeric tolerance for the per-row sum-to-1 check. Defaults to `1e-6`.

## Value

A list of two data frames. `quality` has one row per sample: the
`meta_cols`, `DP`, `DE`, and `dp_coverage` / `de_coverage` (the summed
RRA actually scored for each metric). `dropped` has one row per dropped
taxon-sample: `sample_id`, `rra_taxon`, `rra_value`, `entry_level`, and
`reason` (`unresolved`, `moss`, `na_phenology`, `na_quality_DP`, or
`na_quality_DE`).

## Details

Resolution and fall-through: a taxon column name is matched in `lookup`
as a species (binomial), a genus (the first word of a binomial, or a
single-word name), a family, or an order; `functional_group` and
`growth_form` are taken as the modal value across the matched rows.
Phenology is assigned in precedence order: MOSS in either group field
drops the taxon, TREE forces `T`, FERN forces `FE`, otherwise `mode` is
searched from the taxon's level down to `all` for the first non-`NA`
code that period. Quality is read at `Part == 'ALL'` starting at the
taxon's resolved level and walking up to coarser levels whenever a row
is missing or its value is `NA`. Taxa not found in `lookup`, dropped for
MOSS, lacking any phenology code, or lacking a usable quality value are
excluded and their weight removed; the surviving weights form each
metric's denominator.

## See also

[`calc_phenology_mode`](https://ryanmartinecology.github.io/ecologytools/reference/calc_phenology_mode.md)
