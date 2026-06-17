# Summarize diet proportions from RRA metabarcoding data

Computes per-taxon diet proportion summaries at species, genus, family,
and order resolutions from a sample-by-taxon relative read abundance
(RRA) matrix. Within each sample, RRA values are summed by taxonomic
group; the resulting per-sample proportions are then summarized across
samples as `max + sd`, capped at 1, and returned as `diet_proportion`.

## Usage

``` r
calc_diet_prop(rra, lookup, resolve = c("all", "skip"))
```

## Arguments

- rra:

  A data frame, tibble, or numeric matrix with samples as rows and taxa
  as columns. Column names are taxon identifiers (e.g.,
  `"Abies lasiocarpa"`, `"Pinaceae"`). All columns must be numeric, and
  each row is assumed to be already normalized (e.g., to sum to 1).

- lookup:

  A taxonomy table containing at minimum the columns `Scientific.Name`,
  `Genus`, `Family`, and `Order`. If any of these columns is missing, a
  warning is issued and the affected rank is skipped (returned as `NULL`
  in the output list).

- resolve:

  Either `"all"` (default) or `"skip"`. When `"all"`, columns that
  cannot be resolved to any rank appear in every level's output with
  `diet_proportion = NA_real_`. When `"skip"`, unresolved columns are
  silently dropped.

## Value

A named list with elements `species`, `genus`, `family`, `order`. The
species element has two columns (`species`, `diet_proportion`). The
genus, family, and order elements include an additional logical column
(`genus_in_lookup`, `family_in_lookup`, `order_in_lookup`) indicating
whether the rolled-up taxon name was found in `lookup`. A level whose
required lookup column is absent is returned as `NULL`.

## Details

Each input column is assigned to a rank as follows:

- Species: exact match against `lookup$Scientific.Name`.

- Genus: a two-word column name is parsed to its first whitespace-
  separated token and treated as a genus; a single-word column name is
  used as is. In both cases, the resulting genus contributes to the
  genus, family, and order outputs. The `genus_in_lookup` flag indicates
  whether that genus appears in `lookup$Genus`.

- Family: single-word columns that did not match at genus level are
  checked against `lookup$Family`.

- Order: single-word columns that did not match at genus or family level
  are checked against `lookup$Order`.

Higher-rank rollups for two-word columns are obtained by looking up the
first-token genus in a deduplicated `Genus` → `Family`/`Order` table
(first occurrence retained). Genus-rank columns roll up the same way;
family-rank columns roll up to order via `lookup$Family` →
`lookup$Order` (first occurrence retained). Two-word columns whose first
token is not in `lookup$Genus` will receive `NA` for family/order
rollups and contribute only at genus level (with
`genus_in_lookup = FALSE`).
