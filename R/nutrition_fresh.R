#' Forage Resource Evaluation System for Habitat (FRESH)
#'
#' Computes the maximum suitable forage biomass that meets minimum digestible
#' energy and digestible protein concentration constraints for a generalist
#' herbivore, using linear programming.
#'
#' @param biomass_data Long-format data frame with one row per forage per
#'   patch.
#'   Required columns: `plant_id` (character), `biomass` (numeric).
#'   Optional columns:
#'   * `patch_id` (character or factor) -- groups rows into patches; one LP
#'     is solved per patch. If absent, all rows are treated as a single
#'     patch and a warning is issued.
#'   * `plant_part` or `plant_phenology` (character) -- subdivides a species
#'     into parts with different nutritional values (e.g., leaves vs.
#'     twigs). Must match the column name used in `nutrition_data`. If
#'     absent in both tables, the join uses `plant_id` only.
#'   * `area` (numeric, m^2) -- area over which biomass was collected. Must
#'     be constant within a patch. Overridden by the `area` argument if
#'     both are supplied.
#'   * `habitat_type` (integer, factor, or character) -- habitat
#'     classification for each row. If present, a `$habitat` element is
#'     added to the output with per-habitat-type summaries averaged across
#'     patches. Also propagated to the `$detail` output as a column.
#' @param nutrition_data Lookup data frame, one row per plant_id (and part).
#'   Required columns: `plant_id` (character), `de` (numeric), `dp`
#'   (numeric), `diet_proportion` (numeric, 0--1).
#'   Optional columns:
#'   * `plant_part` or `plant_phenology` (character) -- must match the
#'     column name used in `biomass_data`.
#'   * `sd_de` and `sd_dp` (numeric) -- standard deviations of `de` and
#'     `dp`. Both must be present or neither. If present, each forage is
#'     expanded into low/mid/high SD groups in the LP (proportions 0.16 /
#'     0.68 / 0.16 from the normal distribution), allowing partial
#'     inclusion of forages near a nutritional threshold. Forages with `NA`
#'     SD values stay as a single (mean) group.
#' @param biomass_unit Unit of `biomass` column. `"g"` (default) or `"kg"`.
#' @param de_unit Unit of `de` column and `animal_de_req`. `"kj_g"` (default)
#'   or `"kcal_g"`.
#' @param dmi_unit Unit of `animal_dmi`. `"g_day"` (default) or `"kg_day"`.
#' @param output_area_unit Unit for output biomass densities and animal days.
#'   One of `"kg_ha"` (default), `"g_m2"`, `"g_900m2"`, `"kg_m2"`, or `"g_ha"`.
#' @param area Scalar area in m^2 over which biomass was collected. If
#'   supplied and `biomass_data` has an `area` column, the argument wins (with
#'   warning). At least one of the two must be present.
#' @param animal_de_req Minimum digestible energy concentration of the diet,
#'   in units matching `de_unit`. Scalar or numeric vector. Default 11.5
#'   (kJ/g, corresponding to a moderate-quality diet of roughly 60% dry
#'   matter digestibility for a generalist herbivore). When `animal_de_req`
#'   and `animal_dp_req` are both vectors they must be the same length, and
#'   each pair (`animal_de_req[i]`, `animal_dp_req[i]`) is a separate LP
#'   solve per patch, producing a separate row in `summary` and a separate
#'   set of rows in `detail`. If one is scalar and the other is a vector,
#'   the scalar is recycled to match the vector's length.
#' @param animal_dp_req Minimum digestible protein concentration of the
#'   diet, in g/100g of dry matter. Scalar or numeric vector; recycling and
#'   vector behavior as described for `animal_de_req`. Default 7.5 (an
#'   intermediate value between body-maintenance and lactation requirements
#'   for a medium-sized cervid).
#' @param animal_dmi Daily dry matter intake, in units matching `dmi_unit`.
#'   Optional; if `NULL`, `animal_days_per_area` in the output is `NA`.
#' @param max_any_forage_prop Scalar in \[0, 1\]. Caps the proportion of any
#'   single forage (summed across SD groups) in the diet. Intended to enforce
#'   dietary diversity for generalist herbivores that cannot tolerate any
#'   single forage dominating their diet. Default 1 (no effect).
#' @param min_total_biomass Scalar >= 0, in `output_area_unit`. Subtracted
#'   from total suitable biomass before computing animal days. Acts as a
#'   foraging efficiency floor: below some biomass density, herbivores cannot
#'   forage profitably regardless of forage quality. Default 0 (no effect).
#' @param warnings Logical. If `TRUE` (default), warnings issued during the
#'   call are printed to the console as usual. If `FALSE`, they are
#'   suppressed from the console. Either way, all warning messages are
#'   collected and returned in the `warnings` element of the output object.
#'
#' @return An object of class `"fresh_output"`: a list with the following
#'   components.
#'   * `summary` -- a tibble with one row per patch x constraint pair.
#'     Columns: `patch_id`, `de_req`, `dp_req`, `total_biomass_available`,
#'     `suitable_biomass`, `pct_suitable_biomass`, `animal_days_per_area`,
#'     `mean_de_total`, `mean_dp_total`, `mean_de_suitable`,
#'     `mean_dp_suitable`, `n_forages_used`, `limiting_constraint`. A
#'     logical `infeasible` column is added only if at least one row was
#'     infeasible.
#'   * `detail` -- a tibble with one row per forage per patch per
#'     constraint pair (SD groups collapsed back to the biological forage).
#'     Columns: `patch_id`, `de_req`, `dp_req`, `plant_id`, `plant_part` or
#'     `plant_phenology` (if present in input), `habitat_type` (if present
#'     in input), `biomass_available`, `biomass_used`, `prop_used`,
#'     `prop_of_total`.
#'   * `habitat` -- a tibble with one row per habitat type x constraint
#'     pair, present only when `biomass_data` contains a `habitat_type`
#'     column. Columns: `habitat_type`, `de_req`, `dp_req`,
#'     `mean_total_biomass_per_area`, `mean_suitable_biomass_per_area`,
#'     `mean_de_total`, `mean_dp_total`, `mean_de_suitable`,
#'     `mean_dp_suitable`. Means are simple averages across patches where
#'     that habitat type occurs; patches where it is absent are excluded
#'     from the denominator.
#'   * `call` -- the matched call.
#'   * `inputs` -- a named list of the resolved argument values (units,
#'     area, constraints, and internal flags indicating whether
#'     nutritional variation and habitat summaries were active).
#'   * `data` -- a list with `biomass_data` and `nutrition_data` as supplied
#'     by the user.
#'   * `warnings` -- a character vector of warning messages issued during
#'     the call, or the string `"no warnings"` if none fired.
#'
#' @details
#' This implementation follows the linear-programming formulation of Hanley
#' et al. (2012) but departs from the original in the following ways:
#' * No summer-to-winter conversion. The function is season-agnostic; users
#'   pre-convert biomass and nutritional values for winter analysis.
#' * No snow submodel. The original derives snow depth from elevation,
#'   slope, aspect, and canopy cover; this is not implemented.
#' * No landscape/GIS application. Only stand-level analysis is supported.
#'   Each `patch_id` is solved independently.
#' * Nutritional variation uses normal-distribution probability mass for
#'   the low/mid/high SD groups (0.16 / 0.68 / 0.16) rather than the equal
#'   thirds (1/3 / 1/3 / 1/3) used by Hanley et al. The probability-based
#'   discretization is statistically consistent with the assumption of
#'   normality implied by reporting SDs in the first place.
#' * Infeasible LP solves return `NA` rather than 0, with an `infeasible`
#'   column added to the summary.
#' * `min_total_biomass` is specified in the user's chosen
#'   `output_area_unit` rather than fixed at kg/ha.
#'
#' @references
#' Hanley, T.A.; Spalinger, D.E.; Mock, K.J.; Weaver, O.L.; Harris, G.M. 2012.
#' Forage resource evaluation system for habitat--deer: an interactive deer
#' habitat model. Gen. Tech. Rep. PNW-GTR-858. Portland, OR: U.S. Department
#' of Agriculture, Forest Service, Pacific Northwest Research Station. 64 p.
#' \url{https://research.fs.usda.gov/treesearch/40300}
#'
#' @seealso \code{\link[lpSolve]{lp}} for the underlying linear-programming
#'   solver used internally.
#'
#' @export

fresh <- function(
    biomass_data,
    nutrition_data,
    biomass_unit = c('g', 'kg'),
    de_unit = c('kj_g', 'kcal_g'),
    dmi_unit = c('g_day', 'kg_day'),
    output_area_unit = c('kg_ha', 'g_m2', 'g_900m2', 'kg_m2', 'g_ha'),
    area = NULL,
    animal_de_req = 11.5,
    animal_dp_req = 7.5,
    animal_dmi = NULL,
    max_any_forage_prop = 1,
    min_total_biomass = 0,
    warnings = T
) {

  # ----------------------------------------------------------------------------------------------------------------------
  # warning collection setup
  # ----------------------------------------------------------------------------------------------------------------------

  #1) every warning() call below is intercepted by withCallingHandlers(); messages
  # are appended to `warning_log` and either re-issued (warnings = T) or muffled
  # (warnings = F); the final log is attached to the output object regardless
  warning_log <- character()
  collect_warning <- function(w) {
    warning_log <<- c(warning_log, conditionMessage(w))
    if (!isTRUE(warnings)) invokeRestart('muffleWarning')
  }

  out <- withCallingHandlers(
    {

      # ----------------------------------------------------------------------------------------------------------------------
      # argument matching and scalar validation
      # ----------------------------------------------------------------------------------------------------------------------

      #1) capture the call and match unit arguments
      call <- match.call()
      biomass_unit <- match.arg(arg = biomass_unit)
      de_unit <- match.arg(arg = de_unit)
      dmi_unit <- match.arg(arg = dmi_unit)
      output_area_unit <- match.arg(arg = output_area_unit)

      #2) require data frames
      if (!is.data.frame(biomass_data)) {
        stop('`biomass_data` must be a data frame.',
             call. = F)
      }

      if (!is.data.frame(nutrition_data)) {
        stop('`nutrition_data` must be a data frame.',
             call. = F)
      }

      #3) require numeric, NA-free constraint requirement vectors
      n_de <- length(animal_de_req)
      n_dp <- length(animal_dp_req)

      if (!is.numeric(animal_de_req) || any(is.na(animal_de_req))) {
        stop('`animal_de_req` must be numeric with no NA values.',
             call. = F)
      }

      if (!is.numeric(animal_dp_req) || any(is.na(animal_dp_req))) {
        stop('`animal_dp_req` must be numeric with no NA values.',
             call. = F)
      }

      #4) recycle scalar to vector if one is scalar and the other a vector; error
      # only when both are vectors of different (and non-1) lengths
      if (n_de != n_dp) {
        if (n_de == 1) {
          animal_de_req <- rep(x = animal_de_req, times = n_dp)
        } else if (n_dp == 1) {
          animal_dp_req <- rep(x = animal_dp_req, times = n_de)
        } else {
          stop(
            '`animal_de_req` and `animal_dp_req` must be the same length, or ',
            'one of them must be scalar.',
            call. = F
          )
        }
      }

      #5) require a scalar diversity cap in [0, 1]
      if (length(max_any_forage_prop) != 1 ||
          !is.numeric(max_any_forage_prop) ||
          is.na(max_any_forage_prop) ||
          max_any_forage_prop < 0 ||
          max_any_forage_prop > 1) {
        stop('`max_any_forage_prop` must be a scalar in [0, 1].',
             call. = F)
      }

      #6) require a non-negative scalar biomass floor
      if (length(min_total_biomass) != 1 ||
          !is.numeric(min_total_biomass) ||
          is.na(min_total_biomass) ||
          min_total_biomass < 0) {
        stop('`min_total_biomass` must be a non-negative scalar.',
             call. = F)
      }

      #7) require a positive scalar area when supplied
      if (!is.null(area)) {
        if (length(area) != 1 || !is.numeric(area) ||
            is.na(area) || area <= 0) {
          stop('`area` must be a positive non-zero scalar.',
               call. = F)
        }
      }

      #8) require a positive scalar dry matter intake when supplied
      if (!is.null(animal_dmi)) {
        if (length(animal_dmi) != 1 || !is.numeric(animal_dmi) ||
            is.na(animal_dmi) || animal_dmi <= 0) {
          stop('`animal_dmi` must be a positive scalar or NULL.',
               call. = F)
        }
      }

      # ----------------------------------------------------------------------------------------------------------------------
      # required columns and part/phenology key resolution
      # ----------------------------------------------------------------------------------------------------------------------

      #1) require the biomass_data columns
      missing_b <- setdiff(x = c('plant_id', 'biomass'), y = names(biomass_data))

      if (length(missing_b) > 0) {
        stop(
          '`biomass_data` is missing required columns: ',
          paste(missing_b, collapse = ', '),
          '.', call. = F
        )
      }

      #2) require the nutrition_data columns
      missing_n <- setdiff(x = c('plant_id', 'de', 'dp', 'diet_proportion'),
                           y = names(nutrition_data))

      if (length(missing_n) > 0) {
        stop(
          '`nutrition_data` is missing required columns: ',
          paste(missing_n, collapse = ', '),
          '.', call. = F
        )
      }

      #3) type-check the required biomass_data columns
      if (!is.character(biomass_data$plant_id) &&
          !is.factor(biomass_data$plant_id)) {
        stop('`biomass_data$plant_id` must be character or factor.',
             call. = F)
      }

      if (!is.numeric(biomass_data$biomass)) {
        stop('`biomass_data$biomass` must be numeric.',
             call. = F)
      }

      #4) type-check the required nutrition_data columns
      if (!is.character(nutrition_data$plant_id) &&
          !is.factor(nutrition_data$plant_id)) {
        stop('`nutrition_data$plant_id` must be character or factor.',
             call. = F)
      }

      for (col in c('de', 'dp', 'diet_proportion')) {
        if (!is.numeric(nutrition_data[[col]])) {
          stop('`nutrition_data$', col, '` must be numeric.',
               call. = F)
        }
      }

      #5) type-check the optional biomass_data columns when present
      if ('patch_id' %in% names(biomass_data)) {
        if (!is.character(biomass_data$patch_id) &&
            !is.factor(biomass_data$patch_id)) {
          stop('`biomass_data$patch_id` must be character or factor.',
               call. = F)
        }
      }

      if ('area' %in% names(biomass_data) &&
          !is.numeric(biomass_data$area)) {
        stop('`biomass_data$area` must be numeric.',
             call. = F)
      }

      if ('habitat_type' %in% names(biomass_data)) {
        if (!is.integer(biomass_data$habitat_type) &&
            !is.numeric(biomass_data$habitat_type) &&
            !is.factor(biomass_data$habitat_type) &&
            !is.character(biomass_data$habitat_type)) {
          stop(
            '`biomass_data$habitat_type` must be integer, numeric, factor, ',
            'or character.', call. = F
          )
        }
      }

      #6) type-check the optional nutrition_data SD columns when present
      for (col in c('sd_de', 'sd_dp')) {
        if (col %in% names(nutrition_data) &&
            !is.numeric(nutrition_data[[col]])) {
          stop('`nutrition_data$', col, '` must be numeric.',
               call. = F)
        }
      }

      #7) resolve the part/phenology column name; must be consistent across tables
      b_part_col <- intersect(x = c('plant_part', 'plant_phenology'),
                              y = names(biomass_data))
      n_part_col <- intersect(x = c('plant_part', 'plant_phenology'),
                              y = names(nutrition_data))

      if (length(b_part_col) > 1 || length(n_part_col) > 1) {
        stop(
          'A table contains both `plant_part` and `plant_phenology`. Use one.',
          call. = F
        )
      }

      if (length(b_part_col) != length(n_part_col) ||
          (length(b_part_col) == 1 && b_part_col != n_part_col)) {
        stop(
          '`biomass_data` and `nutrition_data` must use the same part/phenology ',
          'column name (or neither).',
          call. = F
        )
      }

      part_col <- if (length(b_part_col) == 1) b_part_col else NULL
      join_keys <- c('plant_id', part_col)

      #8) type-check the active part/phenology column
      if (!is.null(part_col)) {
        if (!is.character(biomass_data[[part_col]]) &&
            !is.factor(biomass_data[[part_col]])) {
          stop('`biomass_data$', part_col, '` must be character or factor.',
               call. = F)
        }
        if (!is.character(nutrition_data[[part_col]]) &&
            !is.factor(nutrition_data[[part_col]])) {
          stop('`nutrition_data$', part_col, '` must be character or factor.',
               call. = F)
        }
      }

      #9) require nutrition_data to be unique on the join keys
      if (anyDuplicated(nutrition_data[, join_keys, drop = F])) {
        stop(
          '`nutrition_data` has duplicate rows on the join key(s): ',
          paste(join_keys, collapse = ', '), '.', call. = F
        )
      }

      # ----------------------------------------------------------------------------------------------------------------------
      # patch id handling
      # ----------------------------------------------------------------------------------------------------------------------

      #1) supply a single patch when absent; relabel NA patch ids as "unknown"
      if (!'patch_id' %in% names(biomass_data)) {
        warning(
          '`biomass_data` has no `patch_id` column; assuming all rows belong ',
          'to one patch.',
          call. = F
        )
        biomass_data$patch_id <- 'patch_1'
      } else if (any(is.na(biomass_data$patch_id))) {
        n_na <- sum(is.na(biomass_data$patch_id))
        warning(
          n_na, ' row(s) in `biomass_data` have NA `patch_id` values; ',
          "these will be grouped together as patch \"unknown\".",
          call. = F
        )
        # coerce factors to character before assignment to avoid level mismatch
        if (is.factor(biomass_data$patch_id)) {
          biomass_data$patch_id <- as.character(biomass_data$patch_id)
        }
        biomass_data$patch_id[is.na(biomass_data$patch_id)] <- 'unknown'
      }

      # ----------------------------------------------------------------------------------------------------------------------
      # anti-join check
      # ----------------------------------------------------------------------------------------------------------------------

      #1) build join-key strings for both tables
      b_key <- do.call(what = paste, args = c(biomass_data[, join_keys, drop = F],
                                              sep = '\r'))
      n_key <- do.call(what = paste, args = c(nutrition_data[, join_keys, drop = F],
                                              sep = '\r'))

      #2) error on any biomass_data row with no nutrition_data match
      unmatched <- biomass_data[!b_key %in% n_key, , drop = F]
      if (nrow(unmatched) > 0) {
        n_unmatched <- nrow(unmatched)
        show <- utils::head(x = unmatched, n = 20)
        msg <- paste0(
          'The following `biomass_data` rows have no match in `nutrition_data`:\n',
          paste(utils::capture.output(print(show)), collapse = '\n')
        )
        if (n_unmatched > 20) {
          msg <- paste0(msg, '\n(... ', n_unmatched - 20, ' more rows)')
        }
        stop(msg, call. = F)
      }

      # ----------------------------------------------------------------------------------------------------------------------
      # area resolution
      # ----------------------------------------------------------------------------------------------------------------------

      #1) the area argument wins over an area column when both are supplied
      has_area_col <- 'area' %in% names(biomass_data)

      if (!is.null(area) && has_area_col) {
        warning(
          '`area` argument supplied; `area` column in `biomass_data` will be ',
          'ignored.',
          call. = F
        )
        biomass_data$area <- NULL
        has_area_col <- F
      }

      #2) require at least one of the argument or column
      if (is.null(area) && !has_area_col) {
        stop(
          'Either the `area` argument or an `area` column in `biomass_data` ',
          'must be supplied.',
          call. = F
        )
      }

      #3) build per-patch area, checking constancy within each patch when from column
      if (!is.null(area)) {
        patch_area <- data.frame(
          patch_id = unique(biomass_data$patch_id),
          area_m2 = area,
          stringsAsFactors = F
        )
      } else {
        # check constancy of area within each patch
        n_unique <- tapply(X = biomass_data$area, INDEX = biomass_data$patch_id,
                           FUN = function(x) length(unique(stats::na.omit(x))))
        bad <- names(n_unique)[n_unique > 1]
        if (length(bad) > 0) {
          stop(
            'The `area` column varies within patch(es): ',
            paste(bad, collapse = ', '),
            '. Area must be constant within patch.',
            call. = F
          )
        }
        patch_area <- data.frame(
          patch_id = names(n_unique),
          area_m2 = vapply(
            X = split(x = biomass_data$area, f = biomass_data$patch_id),
            FUN = function(x) stats::na.omit(x)[1],
            FUN.VALUE = numeric(1)
          ),
          stringsAsFactors = F
        )
        if (any(is.na(patch_area$area_m2) | patch_area$area_m2 <= 0)) {
          stop(
            '`area` column contains non-positive or all-NA values for some patches.',
            call. = F
          )
        }
      }

      # ----------------------------------------------------------------------------------------------------------------------
      # sd column handling
      # ----------------------------------------------------------------------------------------------------------------------

      #1) require both sd columns or neither, then set the variation flag
      has_sd_de <- 'sd_de' %in% names(nutrition_data)
      has_sd_dp <- 'sd_dp' %in% names(nutrition_data)

      if (has_sd_de != has_sd_dp) {
        stop(
          'Both `sd_de` and `sd_dp` columns must be present in `nutrition_data`, ',
          'or neither.', call. = F
        )
      }

      use_variation <- has_sd_de && has_sd_dp

      # ----------------------------------------------------------------------------------------------------------------------
      # join, internal unit conversion, and sd group expansion
      # ----------------------------------------------------------------------------------------------------------------------

      #1) join nutrition onto biomass and convert biomass to internal grams
      joined <- merge(x = biomass_data, y = nutrition_data,
                      by = join_keys, all.x = T, sort = F)

      biomass_to_g <- if (biomass_unit == 'kg') 1000 else 1
      joined$biomass_g <- joined$biomass * biomass_to_g

      #2) attach per-patch area and compute internal density in g/m^2
      joined <- merge(x = joined, y = patch_area, by = 'patch_id', sort = F)
      joined$density_g_m2 <- joined$biomass_g / joined$area_m2

      #3) convert dry matter intake to internal g/day
      dmi_g_day <- if (!is.null(animal_dmi)) {
        if (dmi_unit == 'kg_day') animal_dmi * 1000 else animal_dmi
      } else {
        NA_real_
      }

      #4) resolve habitat_type: coerce to character for consistent grouping
      use_habitat <- 'habitat_type' %in% names(joined)
      if (use_habitat) {
        joined$habitat_type <- as.character(joined$habitat_type)
      }

      #5) build a stable biological-forage identifier for combined caps and detail
      # aggregation across SD groups
      joined$forage_id <- if (is.null(part_col)) {
        joined$plant_id
      } else {
        paste(joined$plant_id, joined[[part_col]], sep = '__')
      }

      #6) bio_df: one row per patch x forage at the biological level (pre-SD
      # expansion), used for total-biomass weighted mean DE/DP in summary and
      # habitat outputs; keep habitat_type if present
      bio_keep <- c('patch_id', 'forage_id', 'density_g_m2', 'de', 'dp',
                    if (use_habitat) 'habitat_type')
      bio_df <- joined[, bio_keep, drop = F]

      #7) expand into SD groups (or pass through unchanged)
      expanded <- expand_sd_groups(joined, use_variation)

      # ----------------------------------------------------------------------------------------------------------------------
      # output unit conversion factors
      # ----------------------------------------------------------------------------------------------------------------------

      #1) factor from internal g/m^2 to the chosen output area unit
      g_m2_to_out <- switch(
        output_area_unit,
        g_m2 = 1,
        kg_m2 = 1 / 1000,
        kg_ha = 10,
        g_ha = 10000,
        g_900m2 = 900
      )

      #2) min_total_biomass is supplied in output_area_unit; convert to g/m^2
      min_total_g_m2 <- min_total_biomass / g_m2_to_out

      # ----------------------------------------------------------------------------------------------------------------------
      # iterate over patches x constraint pairs
      # ----------------------------------------------------------------------------------------------------------------------

      #1) enumerate patches and constraint pairs
      patches <- unique(expanded$patch_id)

      constraint_pairs <- data.frame(
        de_req = animal_de_req,
        dp_req = animal_dp_req,
        stringsAsFactors = F
      )

      #2) solve one LP per patch x constraint pair and collect summary/detail rows
      summary_rows <- list()
      detail_rows <- list()

      for (pid in patches) {
        patch_df <- expanded[expanded$patch_id == pid, , drop = F]
        patch_bio_df <- bio_df[bio_df$patch_id == pid, , drop = F]
        for (k in seq_len(nrow(constraint_pairs))) {
          de_req_k <- constraint_pairs$de_req[k]
          dp_req_k <- constraint_pairs$dp_req[k]

          lp_res <- solve_one_lp(
            patch_df, de_req_k, dp_req_k, max_any_forage_prop
          )

          detail_rows[[length(detail_rows) + 1]] <- build_detail_row(
            patch_df, lp_res, de_req_k, dp_req_k, g_m2_to_out,
            part_col, use_habitat
          )
          summary_rows[[length(summary_rows) + 1]] <- build_summary_row(
            patch_bio_df, patch_df, lp_res, de_req_k, dp_req_k,
            g_m2_to_out, dmi_g_day, min_total_g_m2, pid
          )
        }
      }

      #3) stack rows, sort alphabetically by patch_id preserving constraint order
      summary_df <- do.call(what = rbind, args = summary_rows)
      detail_df <- do.call(what = rbind, args = detail_rows)

      summary_df <- summary_df[order(summary_df$patch_id), , drop = F]
      detail_df <- detail_df[order(detail_df$patch_id), , drop = F]
      rownames(summary_df) <- NULL
      rownames(detail_df) <- NULL

      #4) drop infeasible column if no patch x constraint pair was infeasible
      if (!any(summary_df$infeasible)) summary_df$infeasible <- NULL

      # ----------------------------------------------------------------------------------------------------------------------
      # build habitat table
      # ----------------------------------------------------------------------------------------------------------------------

      #1) summarize per habitat type only when habitat_type is present
      habitat_df <- if (use_habitat) {
        build_habitat_table(
          bio_df, detail_df, constraint_pairs, g_m2_to_out
        )
      } else {
        NULL
      }

      # ----------------------------------------------------------------------------------------------------------------------
      # assemble fresh_output object
      # ----------------------------------------------------------------------------------------------------------------------

      #1) collect summary, detail, call, resolved inputs, and source data
      out <- list(
        summary = tibble::as_tibble(summary_df),
        detail = tibble::as_tibble(detail_df),
        call = call,
        inputs = list(
          biomass_unit = biomass_unit,
          de_unit = de_unit,
          dmi_unit = dmi_unit,
          output_area_unit = output_area_unit,
          area = area,
          animal_de_req = animal_de_req,
          animal_dp_req = animal_dp_req,
          animal_dmi = animal_dmi,
          max_any_forage_prop = max_any_forage_prop,
          min_total_biomass = min_total_biomass,
          use_variation = use_variation,
          use_habitat = use_habitat,
          part_col = part_col
        ),
        data = list(
          biomass_data = biomass_data,
          nutrition_data = nutrition_data
        )
      )

      #2) attach the habitat table when present and set the output class
      if (use_habitat) out$habitat <- tibble::as_tibble(habitat_df)

      class(out) <- c('fresh_output', 'list')
      out
    },
    warning = collect_warning
  )

  # ----------------------------------------------------------------------------------------------------------------------
  # attach warning log and return
  # ----------------------------------------------------------------------------------------------------------------------

  #1) record the collected warnings, or a sentinel string when none fired
  out$warnings <- if (length(warning_log) == 0) 'no warnings' else warning_log
  out

}


# ----------------------------------------------------------------------------------------------------------------------
# internal helpers
# ----------------------------------------------------------------------------------------------------------------------

#' Expand each forage into low/mid/high SD groups, using normal-distribution
#' probability mass: 0.1587 / 0.6827 / 0.1587. Forages with NA SD values stay
#' as a single (mid) group. Negative adjusted de/dp values are clamped to 0
#' with a single combined warning.
#'
#' @keywords internal
#' @noRd

expand_sd_groups <- function(df,
                             use_variation
) {

  # ----------------------------------------------------------------------------------------------------------------------
  # pass through when variation is off
  # ----------------------------------------------------------------------------------------------------------------------

  #1) treat every forage as a single mid group with full density
  if (!use_variation) {
    df$group <- 'mid'
    df$group_prop <- 1
    df$de_adj <- df$de
    df$dp_adj <- df$dp
    df$density_group_g_m2 <- df$density_g_m2
    return(df)
  }

  # ----------------------------------------------------------------------------------------------------------------------
  # split single-group from multi-group forages
  # ----------------------------------------------------------------------------------------------------------------------

  #1) derive normal-distribution tail and mid probability mass
  p_tail <- stats::pnorm(q = -1)
  p_mid <- 1 - 2 * p_tail

  #2) forages with both SDs present get expanded; others stay single
  has_sd <- !is.na(df$sd_de) & !is.na(df$sd_dp)
  single <- df[!has_sd, , drop = F]
  multi <- df[ has_sd, , drop = F]

  # ----------------------------------------------------------------------------------------------------------------------
  # build the group rows
  # ----------------------------------------------------------------------------------------------------------------------

  #1) single-SD forages become one mid group at full density
  if (nrow(single) > 0) {
    single$group <- 'mid'
    single$group_prop <- 1
    single$de_adj <- single$de
    single$dp_adj <- single$dp
    single$density_group_g_m2 <- single$density_g_m2
  }

  #2) multi-SD forages expand into low/mid/high groups, clamping negatives to 0
  if (nrow(multi) > 0) {
    low <- multi
    low$group <- 'low'
    low$group_prop <- p_tail
    low$de_adj <- pmax(low$de - low$sd_de, 0)
    low$dp_adj <- pmax(low$dp - low$sd_dp, 0)
    low$density_group_g_m2 <- low$density_g_m2 * p_tail

    mid <- multi
    mid$group <- 'mid'
    mid$group_prop <- p_mid
    mid$de_adj <- mid$de
    mid$dp_adj <- mid$dp
    mid$density_group_g_m2 <- mid$density_g_m2 * p_mid

    high <- multi
    high$group <- 'high'
    high$group_prop <- p_tail
    high$de_adj <- high$de + high$sd_de
    high$dp_adj <- high$dp + high$sd_dp
    high$density_group_g_m2 <- high$density_g_m2 * p_tail

    clamped <- (multi$de - multi$sd_de < 0) | (multi$dp - multi$sd_dp < 0)
    if (any(clamped, na.rm = T)) {
      warning(
        'Some low-group de/dp values were clamped to 0 because mean - SD ',
        'was negative.', call. = F
      )
    }
    multi <- rbind(low, mid, high)
  }
  rbind(single, multi)
}


#' Solve one LP for one patch and one constraint pair.
#' Returns a list with the solution vector, infeasibility flag, and the
#' identity of the binding constraint (de / dp / biomass).
#'
#' @keywords internal
#' @noRd

solve_one_lp <- function(df,
                         de_req,
                         dp_req,
                         max_any_forage_prop,
                         .tol = 1e-6) {

  # ----------------------------------------------------------------------------------------------------------------------
  # extract problem inputs
  # ----------------------------------------------------------------------------------------------------------------------

  #1) pull dimensions, bounds, nutrition vectors, and per-forage diet caps
  n <- nrow(df)
  b <- df$density_group_g_m2
  de <- df$de_adj
  dp <- df$dp_adj
  forages <- unique(df$forage_id)
  diet_lk <- tapply(X = df$diet_proportion, INDEX = df$forage_id, FUN = function(x) x[1])

  # ----------------------------------------------------------------------------------------------------------------------
  # build the objective and constraints
  # ----------------------------------------------------------------------------------------------------------------------

  #1) objective: maximize sum(x)
  objective <- rep(x = 1, times = n)

  #2) initialize the constraint matrix accumulators
  constr_rows <- list()
  constr_dir <- character()
  constr_rhs <- numeric()

  #3) availability: x_i <= b_i
  avail_mat <- diag(n)
  constr_rows <- c(constr_rows, split(x = avail_mat, f = seq_len(n)))
  constr_dir <- c(constr_dir, rep(x = '<=', times = n))
  constr_rhs <- c(constr_rhs, b)

  #4) DE concentration: sum((de_i - de_req) x_i) >= 0
  constr_rows <- c(constr_rows, list(de - de_req))
  constr_dir <- c(constr_dir, '>=')
  constr_rhs <- c(constr_rhs, 0)

  #5) DP concentration: sum((dp_i - dp_req) x_i) >= 0
  constr_rows <- c(constr_rows, list(dp - dp_req))
  constr_dir <- c(constr_dir, '>=')
  constr_rhs <- c(constr_rhs, 0)

  #6) per-forage diet_proportion caps (combined across SD groups)
  for (f in forages) {
    p_f <- diet_lk[[f]]
    if (is.na(p_f) || p_f >= 1) next
    mask <- as.numeric(df$forage_id == f)
    constr_rows <- c(constr_rows, list(mask - p_f))
    constr_dir <- c(constr_dir, '<=')
    constr_rhs <- c(constr_rhs, 0)
  }

  #7) generic max_any_forage_prop cap (combined across SD groups)
  if (max_any_forage_prop < 1) {
    for (f in forages) {
      mask <- as.numeric(df$forage_id == f)
      constr_rows <- c(constr_rows, list(mask - max_any_forage_prop))
      constr_dir <- c(constr_dir, '<=')
      constr_rhs <- c(constr_rhs, 0)
    }
  }

  #8) assemble the matrix and solve
  constr_mat <- do.call(what = rbind, args = constr_rows)

  lp_out <- lpSolve::lp(
    direction = 'max',
    objective.in = objective,
    const.mat = constr_mat,
    const.dir = constr_dir,
    const.rhs = constr_rhs
  )

  infeasible <- lp_out$status != 0
  x <- if (infeasible) rep(x = NA_real_, times = n) else lp_out$solution

  # ----------------------------------------------------------------------------------------------------------------------
  # identify the binding constraint
  # ----------------------------------------------------------------------------------------------------------------------

  #1) determine which of DE, DP, total availability, or diet cap is at its limit
  limiting <- NA_character_

  if (!infeasible) {

    total_used <- sum(x)
    total_avail <- sum(b)

    if (total_used > 0) {
      mean_de_sol <- sum(de * x) / total_used
      mean_dp_sol <- sum(dp * x) / total_used
      at_de <- abs(mean_de_sol - de_req) < .tol * max(1, de_req)
      at_dp <- abs(mean_dp_sol - dp_req) < .tol * max(1, dp_req)
      at_biomass <- abs(total_used - total_avail) < .tol * max(1, total_avail)

      # check if any forage is at its diet_proportion or max_any_forage_prop
      # cap (combined across SD groups)
      at_diet_cap <- any(vapply(X = forages, FUN = function(f) {
        used_f <- sum(x[df$forage_id == f])
        prop_f <- if (total_used > 0) used_f / total_used else 0
        cap_f <- min(diet_lk[[f]], max_any_forage_prop, na.rm = T)
        abs(prop_f - cap_f) < .tol
      }, FUN.VALUE = logical(1)))

      # priority: biomass > de > dp > diet_cap
      limiting <- if (at_biomass) {
        'biomass'
      } else if (at_de) {
        'de'
      } else if (at_dp) {
        'dp'
      } else if (at_diet_cap) {
        'diet_cap'
      } else {
        NA_character_
      }
    } else {
      limiting <- 'biomass'
    }
  }

  list(x = x, infeasible = infeasible, limiting = limiting)
}


#' Build the per-patch summary row.
#'
#' @keywords internal
#' @noRd

build_summary_row <- function(patch_bio_df,
                              patch_df,
                              lp_res,
                              de_req,
                              dp_req,
                              g_m2_to_out,
                              dmi_g_day,
                              min_total_g_m2,
                              pid
) {

  # ----------------------------------------------------------------------------------------------------------------------
  # total-biomass weighted means
  # ----------------------------------------------------------------------------------------------------------------------

  #1) biological forage level, all forages in patch
  total_bio_g_m2 <- sum(patch_bio_df$density_g_m2)
  total_avail_out <- round(x = total_bio_g_m2 * g_m2_to_out, digits = 2)

  if (total_bio_g_m2 > 0) {
    mean_de_total <- round(
      x = sum(patch_bio_df$de * patch_bio_df$density_g_m2) / total_bio_g_m2, digits = 2
    )
    mean_dp_total <- round(
      x = sum(patch_bio_df$dp * patch_bio_df$density_g_m2) / total_bio_g_m2, digits = 2
    )
  } else {
    mean_de_total <- NA_real_
    mean_dp_total <- NA_real_
  }

  # ----------------------------------------------------------------------------------------------------------------------
  # assemble the summary row
  # ----------------------------------------------------------------------------------------------------------------------

  #1) infeasible solve: NA the suitable-side columns
  if (lp_res$infeasible) {
    data.frame(
      patch_id = pid,
      de_req = de_req,
      dp_req = dp_req,
      total_biomass_available = total_avail_out,
      suitable_biomass = NA_real_,
      pct_suitable_biomass = NA_real_,
      animal_days_per_area = NA_real_,
      mean_de_total = mean_de_total,
      mean_dp_total = mean_dp_total,
      mean_de_suitable = NA_real_,
      mean_dp_suitable = NA_real_,
      n_forages_used = NA_integer_,
      limiting_constraint = NA_character_,
      infeasible = T,
      stringsAsFactors = F
    )
  } else {
    #2) feasible solve: compute suitable biomass, weighted means, and animal days
    suitable_g_m2 <- sum(lp_res$x)
    suitable_out <- round(x = suitable_g_m2 * g_m2_to_out, digits = 2)
    pct_suitable <- round(
      x = if (total_bio_g_m2 > 0) suitable_g_m2 / total_bio_g_m2 * 100
      else NA_real_,
      digits = 2
    )
    if (suitable_g_m2 > 0) {
      mean_de_suitable <- round(
        x = sum(patch_df$de_adj * lp_res$x) / suitable_g_m2, digits = 2
      )
      mean_dp_suitable <- round(
        x = sum(patch_df$dp_adj * lp_res$x) / suitable_g_m2, digits = 2
      )
      used_per_forage <- tapply(X = lp_res$x, INDEX = patch_df$forage_id, FUN = sum)
      n_forages <- sum(used_per_forage > 1e-9)
    } else {
      mean_de_suitable <- NA_real_
      mean_dp_suitable <- NA_real_
      n_forages <- 0L
    }
    animal_days <- if (!is.na(dmi_g_day)) {
      adj_g_m2 <- max(suitable_g_m2 - min_total_g_m2, 0)
      round(x = (adj_g_m2 / dmi_g_day) * g_m2_to_out, digits = 2)
    } else {
      NA_real_
    }
    data.frame(
      patch_id = pid,
      de_req = de_req,
      dp_req = dp_req,
      total_biomass_available = total_avail_out,
      suitable_biomass = suitable_out,
      pct_suitable_biomass = pct_suitable,
      animal_days_per_area = animal_days,
      mean_de_total = mean_de_total,
      mean_dp_total = mean_dp_total,
      mean_de_suitable = mean_de_suitable,
      mean_dp_suitable = mean_dp_suitable,
      n_forages_used = as.integer(n_forages),
      limiting_constraint = lp_res$limiting,
      infeasible = F,
      stringsAsFactors = F
    )
  }
}


#' Build per-forage detail rows for one patch x constraint pair, collapsing
#' SD groups back to the biological forage.
#'
#' @keywords internal
#' @noRd

build_detail_row <- function(patch_df,
                             lp_res,
                             de_req,
                             dp_req,
                             g_m2_to_out,
                             part_col,
                             use_habitat
) {

  # ----------------------------------------------------------------------------------------------------------------------
  # aggregate sd groups back to biological forage
  # ----------------------------------------------------------------------------------------------------------------------

  #1) attach the solution and sum density/usage over the SD groups per forage
  patch_df$x_used <- lp_res$x

  agg_keys <- c('patch_id', 'plant_id',
                if (!is.null(part_col)) part_col,
                if (use_habitat) 'habitat_type',
                'forage_id')
  agg <- stats::aggregate(
    x = patch_df[, c('density_group_g_m2', 'x_used')],
    by = patch_df[, agg_keys, drop = F],
    FUN = function(v) if (all(is.na(v))) NA_real_ else sum(v, na.rm = T)
  )

  # ----------------------------------------------------------------------------------------------------------------------
  # compute output-unit columns
  # ----------------------------------------------------------------------------------------------------------------------

  #1) convert available and used biomass to output units
  agg$biomass_available <- round(x = agg$density_group_g_m2 * g_m2_to_out, digits = 2)
  agg$biomass_used <- round(x = agg$x_used * g_m2_to_out, digits = 2)

  total_used_g_m2 <- if (lp_res$infeasible) NA_real_ else sum(lp_res$x)

  #2) proportion of each forage's availability used
  agg$prop_used <- round(x = ifelse(
    test = is.na(agg$biomass_used) | agg$biomass_available == 0,
    yes = NA_real_,
    no = agg$biomass_used / agg$biomass_available
  ), digits = 2)

  #3) proportion of total diet drawn from each forage
  agg$prop_of_total <- round(
    x = if (lp_res$infeasible ||
            is.na(total_used_g_m2) ||
            total_used_g_m2 == 0) {
      rep(x = NA_real_, times = nrow(agg))
    } else {
      agg$x_used / total_used_g_m2
    },
    digits = 2
  )

  agg$de_req <- de_req
  agg$dp_req <- dp_req

  # ----------------------------------------------------------------------------------------------------------------------
  # drop helpers and order columns
  # ----------------------------------------------------------------------------------------------------------------------

  #1) drop internal helpers and reorder columns
  agg$density_group_g_m2 <- NULL
  agg$x_used <- NULL
  agg$forage_id <- NULL

  col_order <- c('patch_id', 'de_req', 'dp_req', 'plant_id',
                 if (!is.null(part_col)) part_col,
                 if (use_habitat) 'habitat_type',
                 'biomass_available', 'biomass_used',
                 'prop_used', 'prop_of_total')
  agg[, col_order, drop = F]
}


#' Build the habitat-type summary table.
#'
#' Averages per-patch values across patches for each habitat_type x
#' constraint pair. Patches where a habitat type is absent are excluded
#' from the patch-count denominator.
#'
#' @param bio_df Biological-forage-level data frame (pre-SD expansion) with
#'   columns patch_id, forage_id, density_g_m2, de, dp, habitat_type.
#' @param detail_df The assembled detail tibble (all patches, all constraint
#'   pairs) with columns patch_id, de_req, dp_req, habitat_type,
#'   biomass_available (in output units), biomass_used (in output units).
#' @param constraint_pairs Data frame with de_req and dp_req columns.
#' @param g_m2_to_out Conversion factor from g/m^2 to output area unit.
#'
#' @keywords internal
#' @noRd

build_habitat_table <- function(bio_df,
                                detail_df,
                                constraint_pairs,
                                g_m2_to_out
) {

  # ----------------------------------------------------------------------------------------------------------------------
  # iterate over habitat types
  # ----------------------------------------------------------------------------------------------------------------------

  #1) enumerate habitat types and prepare the row accumulator
  habitat_types <- sort(unique(bio_df$habitat_type))
  rows <- list()

  for (ht in habitat_types) {
    bio_ht <- bio_df[bio_df$habitat_type == ht, , drop = F]

    # ----------------------------------------------------------------------------------------------------------------------
    # per-patch totals for this habitat type
    # ----------------------------------------------------------------------------------------------------------------------

    #1) patches that contain this habitat type
    patches_ht <- unique(bio_ht$patch_id)
    n_patches <- length(patches_ht)

    #2) per-patch total biomass density and weighted mean DE/DP (total)
    total_bio_per_patch <- vapply(X = patches_ht, FUN = function(pid) {
      d <- bio_ht[bio_ht$patch_id == pid, , drop = F]
      sum(d$density_g_m2) * g_m2_to_out
    }, FUN.VALUE = numeric(1))

    wt_de_total_per_patch <- vapply(X = patches_ht, FUN = function(pid) {
      d <- bio_ht[bio_ht$patch_id == pid, , drop = F]
      w <- sum(d$density_g_m2)
      if (w > 0) sum(d$de * d$density_g_m2) / w else NA_real_
    }, FUN.VALUE = numeric(1))

    wt_dp_total_per_patch <- vapply(X = patches_ht, FUN = function(pid) {
      d <- bio_ht[bio_ht$patch_id == pid, , drop = F]
      w <- sum(d$density_g_m2)
      if (w > 0) sum(d$dp * d$density_g_m2) / w else NA_real_
    }, FUN.VALUE = numeric(1))

    mean_total_bio <- round(x = mean(total_bio_per_patch), digits = 2)
    mean_de_total <- round(x = mean(x = wt_de_total_per_patch, na.rm = T), digits = 2)
    mean_dp_total <- round(x = mean(x = wt_dp_total_per_patch, na.rm = T), digits = 2)

    # ----------------------------------------------------------------------------------------------------------------------
    # per-constraint suitable summaries
    # ----------------------------------------------------------------------------------------------------------------------

    for (k in seq_len(nrow(constraint_pairs))) {
      de_req_k <- constraint_pairs$de_req[k]
      dp_req_k <- constraint_pairs$dp_req[k]

      #1) pull suitable biomass per patch from detail_df for this ht x constraint
      det_k <- detail_df[
        detail_df$de_req == de_req_k &
          detail_df$dp_req == dp_req_k &
          detail_df$habitat_type == ht &
          detail_df$patch_id %in% patches_ht, , drop = F
      ]

      #2) per-patch suitable biomass; biomass_used in detail is already in output
      # units, weighted directly as it is proportional to g/m^2
      suit_per_patch <- vapply(X = patches_ht, FUN = function(pid) {
        d <- det_k[det_k$patch_id == pid, , drop = F]
        sum(d$biomass_used, na.rm = T)
      }, FUN.VALUE = numeric(1))

      #3) weighted mean DE/DP suitable: join back to bio_df for de/dp values,
      # using biomass_used as weights, matched on patch_id x forage_id via detail
      wt_de_suit_per_patch <- vapply(X = patches_ht, FUN = function(pid) {
        d <- det_k[det_k$patch_id == pid, , drop = F]
        w <- d$biomass_used
        w[is.na(w)] <- 0
        total_w <- sum(w)
        if (total_w == 0) return(NA_real_)
        bio_p <- bio_df[bio_df$patch_id == pid &
                          bio_df$habitat_type == ht, , drop = F]
        if (nrow(bio_p) == 0 || sum(bio_p$density_g_m2) == 0) return(NA_real_)
        sum(bio_p$de * bio_p$density_g_m2) / sum(bio_p$density_g_m2)
      }, FUN.VALUE = numeric(1))

      wt_dp_suit_per_patch <- vapply(X = patches_ht, FUN = function(pid) {
        d <- det_k[det_k$patch_id == pid, , drop = F]
        w <- d$biomass_used
        w[is.na(w)] <- 0
        total_w <- sum(w)
        if (total_w == 0) return(NA_real_)
        bio_p <- bio_df[bio_df$patch_id == pid &
                          bio_df$habitat_type == ht, , drop = F]
        if (nrow(bio_p) == 0 || sum(bio_p$density_g_m2) == 0) return(NA_real_)
        sum(bio_p$dp * bio_p$density_g_m2) / sum(bio_p$density_g_m2)
      }, FUN.VALUE = numeric(1))

      mean_suit_bio <- round(x = mean(x = suit_per_patch, na.rm = T), digits = 2)
      mean_de_suitable <- round(x = mean(x = wt_de_suit_per_patch, na.rm = T), digits = 2)
      mean_dp_suitable <- round(x = mean(x = wt_dp_suit_per_patch, na.rm = T), digits = 2)

      #4) emit one row per habitat type x constraint pair
      rows[[length(rows) + 1]] <- data.frame(
        habitat_type = ht,
        de_req = de_req_k,
        dp_req = dp_req_k,
        mean_total_biomass_per_area = mean_total_bio,
        mean_suitable_biomass_per_area = mean_suit_bio,
        mean_de_total = mean_de_total,
        mean_dp_total = mean_dp_total,
        mean_de_suitable = mean_de_suitable,
        mean_dp_suitable = mean_dp_suitable,
        stringsAsFactors = F
      )
    }
  }

  # ----------------------------------------------------------------------------------------------------------------------
  # stack the result
  # ----------------------------------------------------------------------------------------------------------------------

  #1) bind rows and clear row names
  result <- do.call(what = rbind, args = rows)
  rownames(result) <- NULL
  result
}
