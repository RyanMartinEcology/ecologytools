# Summarize plant forage quality at species, genus, family, functional group, and growth form levels

Computes mean, standard deviation, and non-NA sample size for digestible
energy (DE), digestible protein (DP), and dry matter digestibility (DMD)
at up to five resolutions. Higher-level summaries are computed directly
from the raw input rows. Where a higher-level grouping spans multiple
parent values, the first occurrence is retained.

## Usage

``` r
plant_quality_summary(data)
```

## Arguments

- data:

  A data frame containing `DE`, `DP`, `DMD`, and at least one of `Code`,
  `Genus`, `Family`, `Functional.Group`, or `Growth.Form`. May
  optionally contain `Scientific.Name`, `Phenology`, and `Part`.

## Value

A named list with elements `species`, `genus`, `family`,
`functional_group`, `growth_form`. Each is either a tibble of summaries
or `NULL` if the corresponding column was absent from `data`.

## Details

Grouping levels (`Code`/`Scientific.Name`, `Genus`, `Family`,
`Functional.Group`, `Growth.Form`) and optional grouping columns
(`Phenology`, `Part`) are used only if present in `data`. At least one
of `Code`, `Genus`, `Family`, `Functional.Group`, or `Growth.Form` must
be present. Levels not represented in the data are returned as `NULL`.
