# Forage Resource Evaluation System for Habitat (FRESH)

Computes the maximum suitable forage biomass that meets minimum
digestible energy and digestible protein concentration constraints for a
generalist herbivore, using linear programming.

## Usage

``` r
fresh(
  biomass_data,
  nutrition_data,
  biomass_unit = c("g", "kg"),
  de_unit = c("kj_g", "kcal_g"),
  dmi_unit = c("g_day", "kg_day"),
  output_area_unit = c("kg_ha", "g_m2", "g_900m2", "kg_m2", "g_ha"),
  area = NULL,
  animal_de_req = 11.5,
  animal_dp_req = 7.5,
  animal_dmi = NULL,
  max_any_forage_prop = 1,
  min_total_biomass = 0,
  warnings = T
)
```

## Arguments

- biomass_data:

  Long-format data frame with one row per forage per patch. Required
  columns: `plant_id` (character), `biomass` (numeric). Optional
  columns:

  - `patch_id` (character or factor) – groups rows into patches; one LP
    is solved per patch. If absent, all rows are treated as a single
    patch and a warning is issued.

  - `plant_part` or `plant_phenology` (character) – subdivides a species
    into parts with different nutritional values (e.g., leaves vs.
    twigs). Must match the column name used in `nutrition_data`. If
    absent in both tables, the join uses `plant_id` only.

  - `area` (numeric, m^2) – area over which biomass was collected. Must
    be constant within a patch. Overridden by the `area` argument if
    both are supplied.

  - `habitat_type` (integer, factor, or character) – habitat
    classification for each row. If present, a `$habitat` element is
    added to the output with per-habitat-type summaries averaged across
    patches. Also propagated to the `$detail` output as a column.

- nutrition_data:

  Lookup data frame, one row per plant_id (and part). Required columns:
  `plant_id` (character), `de` (numeric), `dp` (numeric),
  `diet_proportion` (numeric, 0–1). Optional columns:

  - `plant_part` or `plant_phenology` (character) – must match the
    column name used in `biomass_data`.

  - `sd_de` and `sd_dp` (numeric) – standard deviations of `de` and
    `dp`. Both must be present or neither. If present, each forage is
    expanded into low/mid/high SD groups in the LP (proportions 0.16 /
    0.68 / 0.16 from the normal distribution), allowing partial
    inclusion of forages near a nutritional threshold. Forages with `NA`
    SD values stay as a single (mean) group.

- biomass_unit:

  Unit of `biomass` column. `"g"` (default) or `"kg"`.

- de_unit:

  Unit of `de` column and `animal_de_req`. `"kj_g"` (default) or
  `"kcal_g"`.

- dmi_unit:

  Unit of `animal_dmi`. `"g_day"` (default) or `"kg_day"`.

- output_area_unit:

  Unit for output biomass densities and animal days. One of `"kg_ha"`
  (default), `"g_m2"`, `"g_900m2"`, `"kg_m2"`, or `"g_ha"`.

- area:

  Scalar area in m^2 over which biomass was collected. If supplied and
  `biomass_data` has an `area` column, the argument wins (with warning).
  At least one of the two must be present.

- animal_de_req:

  Minimum digestible energy concentration of the diet, in units matching
  `de_unit`. Scalar or numeric vector. Default 11.5 (kJ/g, corresponding
  to a moderate-quality diet of roughly 60% dry matter digestibility for
  a generalist herbivore). When `animal_de_req` and `animal_dp_req` are
  both vectors they must be the same length, and each pair
  (`animal_de_req[i]`, `animal_dp_req[i]`) is a separate LP solve per
  patch, producing a separate row in `summary` and a separate set of
  rows in `detail`. If one is scalar and the other is a vector, the
  scalar is recycled to match the vector's length.

- animal_dp_req:

  Minimum digestible protein concentration of the diet, in g/100g of dry
  matter. Scalar or numeric vector; recycling and vector behavior as
  described for `animal_de_req`. Default 7.5 (an intermediate value
  between body-maintenance and lactation requirements for a medium-sized
  cervid).

- animal_dmi:

  Daily dry matter intake, in units matching `dmi_unit`. Optional; if
  `NULL`, `animal_days_per_area` in the output is `NA`.

- max_any_forage_prop:

  Scalar in \[0, 1\]. Caps the proportion of any single forage (summed
  across SD groups) in the diet. Intended to enforce dietary diversity
  for generalist herbivores that cannot tolerate any single forage
  dominating their diet. Default 1 (no effect).

- min_total_biomass:

  Scalar \>= 0, in `output_area_unit`. Subtracted from total suitable
  biomass before computing animal days. Acts as a foraging efficiency
  floor: below some biomass density, herbivores cannot forage profitably
  regardless of forage quality. Default 0 (no effect).

- warnings:

  Logical. If `TRUE` (default), warnings issued during the call are
  printed to the console as usual. If `FALSE`, they are suppressed from
  the console. Either way, all warning messages are collected and
  returned in the `warnings` element of the output object.

## Value

An object of class `"fresh_output"`: a list with the following
components.

- `summary` – a tibble with one row per patch x constraint pair.
  Columns: `patch_id`, `de_req`, `dp_req`, `total_biomass_available`,
  `suitable_biomass`, `pct_suitable_biomass`, `animal_days_per_area`,
  `mean_de_total`, `mean_dp_total`, `mean_de_suitable`,
  `mean_dp_suitable`, `n_forages_used`, `limiting_constraint`. A logical
  `infeasible` column is added only if at least one row was infeasible.

- `detail` – a tibble with one row per forage per patch per constraint
  pair (SD groups collapsed back to the biological forage). Columns:
  `patch_id`, `de_req`, `dp_req`, `plant_id`, `plant_part` or
  `plant_phenology` (if present in input), `habitat_type` (if present in
  input), `biomass_available`, `biomass_used`, `prop_used`,
  `prop_of_total`.

- `habitat` – a tibble with one row per habitat type x constraint pair,
  present only when `biomass_data` contains a `habitat_type` column.
  Columns: `habitat_type`, `de_req`, `dp_req`,
  `mean_total_biomass_per_area`, `mean_suitable_biomass_per_area`,
  `mean_de_total`, `mean_dp_total`, `mean_de_suitable`,
  `mean_dp_suitable`. Means are simple averages across patches where
  that habitat type occurs; patches where it is absent are excluded from
  the denominator.

- `call` – the matched call.

- `inputs` – a named list of the resolved argument values (units, area,
  constraints, and internal flags indicating whether nutritional
  variation and habitat summaries were active).

- `data` – a list with `biomass_data` and `nutrition_data` as supplied
  by the user.

- `warnings` – a character vector of warning messages issued during the
  call, or the string `"no warnings"` if none fired.

## Details

This implementation follows the linear-programming formulation of Hanley
et al. (2012) but departs from the original in the following ways:

- No summer-to-winter conversion. The function is season-agnostic; users
  pre-convert biomass and nutritional values for winter analysis.

- No snow submodel. The original derives snow depth from elevation,
  slope, aspect, and canopy cover; this is not implemented.

- No landscape/GIS application. Only stand-level analysis is supported.
  Each `patch_id` is solved independently.

- Nutritional variation uses normal-distribution probability mass for
  the low/mid/high SD groups (0.16 / 0.68 / 0.16) rather than the equal
  thirds (1/3 / 1/3 / 1/3) used by Hanley et al. The probability-based
  discretization is statistically consistent with the assumption of
  normality implied by reporting SDs in the first place.

- Infeasible LP solves return `NA` rather than 0, with an `infeasible`
  column added to the summary.

- `min_total_biomass` is specified in the user's chosen
  `output_area_unit` rather than fixed at kg/ha.

## References

Hanley, T.A.; Spalinger, D.E.; Mock, K.J.; Weaver, O.L.; Harris, G.M.
2012. Forage resource evaluation system for habitat–deer: an interactive
deer habitat model. Gen. Tech. Rep. PNW-GTR-858. Portland, OR: U.S.
Department of Agriculture, Forest Service, Pacific Northwest Research
Station. 64 p. <https://research.fs.usda.gov/treesearch/40300>

## See also

[`lp`](https://rdrr.io/pkg/lpSolve/man/lp.html) for the underlying
linear-programming solver used internally.
