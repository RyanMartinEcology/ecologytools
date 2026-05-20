#' Summarize plant forage quality at species, genus, family, and order levels
#'
#' Computes mean, standard deviation, and non-NA sample size for digestible
#' energy (DE), digestible protein (DP), and dry matter digestibility (DMD)
#' at up to four taxonomic resolutions. Higher-level summaries are computed
#' directly from the raw input rows. Where a higher-level taxon spans
#' multiple parent values, the first occurrence is retained.
#'
#' Taxonomic levels (`Code`/`Scientific.Name`, `Genus`, `Family`, `Order`) and
#' optional grouping columns (`Phenology`, `Part`) are used only if present in
#' `data`. At least one of `Code`, `Genus`, `Family`, or `Order` must be
#' present. Levels not represented in the data are returned as `NULL`.
#'
#' @param data A data frame containing `DE`, `DP`, `DMD`, and at least one of
#'   `Code`, `Genus`, `Family`, or `Order`. May optionally contain
#'   `Scientific.Name`, `Phenology`, and `Part`.
#'
#' @return A named list with elements `species`, `genus`, `family`, `order`.
#'   Each is either a tibble of summaries or `NULL` if the corresponding
#'   taxonomic column was absent from `data`.
#'
#' @importFrom dplyr group_by summarise left_join select distinct everything all_of across
#' @importFrom rlang .data
#' @export
plant_quality_summary <- function(data) {

  cols <- names(data)

  # Identify which taxonomic levels and optional groupers are available.
  has_species <- "Code"   %in% cols
  has_genus   <- "Genus"  %in% cols
  has_family  <- "Family" %in% cols
  has_order   <- "Order"  %in% cols

  if (!any(has_species, has_genus, has_family, has_order)) {
    stop("`data` must contain at least one of: Code, Genus, Family, Order.")
  }

  opt_groupers <- intersect(c("Phenology", "Part"), cols)

  # Helper: compute mean, sd, and non-NA n for DE, DP, DMD.
  qual_stats <- function(df) {
    dplyr::summarise(
      df,
      DE_mean  = mean(.data$DE,       na.rm = TRUE),
      DE_sd    = stats::sd(.data$DE,  na.rm = TRUE),
      DE_n     = sum(!is.na(.data$DE)),
      DP_mean  = mean(.data$DP,       na.rm = TRUE),
      DP_sd    = stats::sd(.data$DP,  na.rm = TRUE),
      DP_n     = sum(!is.na(.data$DP)),
      DMD_mean = mean(.data$DMD,      na.rm = TRUE),
      DMD_sd   = stats::sd(.data$DMD, na.rm = TRUE),
      DMD_n    = sum(!is.na(.data$DMD)),
      .groups  = "drop"
    )
  }

  # Helper: summarize one level. `level_col` is the primary grouping column;
  # `carry` is a character vector of higher-rank metadata columns to attach
  # via first-occurrence join.
  summarize_level <- function(level_col, carry) {

    carry <- intersect(carry, cols)
    groupers <- c(level_col, opt_groupers)

    out <- data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(groupers))) |>
      qual_stats()

    if (length(carry) > 0) {
      meta <- data |>
        dplyr::select(dplyr::all_of(c(level_col, carry))) |>
        dplyr::distinct(.data[[level_col]], .keep_all = TRUE)
      out <- dplyr::left_join(out, meta, by = level_col)
    }

    front <- c(level_col, carry, opt_groupers)
    out |> dplyr::select(dplyr::all_of(front), dplyr::everything())
  }

  # Species level: carry Scientific.Name, Genus, Family, Order if present.
  species_summary <- if (has_species) {
    summarize_level("Code", c("Scientific.Name", "Genus", "Family", "Order"))
  } else NULL

  # Genus level: carry Family, Order if present.
  genus_summary <- if (has_genus) {
    summarize_level("Genus", c("Family", "Order"))
  } else NULL

  # Family level: carry Order if present.
  family_summary <- if (has_family) {
    summarize_level("Family", "Order")
  } else NULL

  # Order level: nothing to carry.
  order_summary <- if (has_order) {
    summarize_level("Order", character(0))
  } else NULL

  list(
    species = species_summary,
    genus   = genus_summary,
    family  = family_summary,
    order   = order_summary
  )
}

#' Summarize diet proportions from RRA metabarcoding data
#'
#' Computes per-taxon diet proportion summaries at species, genus, family,
#' and order resolutions from a sample-by-taxon relative read abundance (RRA)
#' matrix. Within each sample, RRA values are summed by taxonomic group; the
#' resulting per-sample proportions are then summarized across samples as
#' `max + sd`, capped at 1, and returned as `diet_proportion`.
#'
#' @param rra A data frame, tibble, or numeric matrix with samples as rows
#'   and taxa as columns. Column names are taxon identifiers (e.g.,
#'   `"Abies lasiocarpa"`, `"Pinaceae"`). All columns must be numeric, and
#'   each row is assumed to be already normalized (e.g., to sum to 1).
#' @param lookup A taxonomy table containing at minimum the columns
#'   `Scientific.Name`, `Genus`, `Family`, and `Order`. If any of these
#'   columns is missing, a warning is issued and the affected rank is
#'   skipped (returned as `NULL` in the output list).
#' @param resolve Either `"all"` (default) or `"skip"`. When `"all"`, columns
#'   that cannot be resolved to any rank appear in every level's output with
#'   `diet_proportion = NA_real_`. When `"skip"`, unresolved columns are
#'   silently dropped.
#'
#' @return A named list with elements `species`, `genus`, `family`, `order`.
#'   The species element has two columns (`species`, `diet_proportion`). The
#'   genus, family, and order elements include an additional logical column
#'   (`genus_in_lookup`, `family_in_lookup`, `order_in_lookup`) indicating
#'   whether the rolled-up taxon name was found in `lookup`. A level whose
#'   required lookup column is absent is returned as `NULL`.
#'
#' @details
#' Each input column is assigned to its lowest-resolution rank by exact
#' matching against `lookup`: first `Scientific.Name` (species), then
#' `Genus`, `Family`, and `Order`. Columns matched at species level
#' contribute to species, genus, family, and order; columns matched at
#' genus contribute to genus, family, and order; family-level columns to
#' family and order; order-level columns to order only.
#'
#' For each species-level column, the parent genus, family, and order are
#' obtained from the species' row in `lookup` (first occurrence retained
#' when a species spans multiple rows). For each genus-level column, the
#' parent family and order are obtained by deduplicating `lookup` by
#' `Genus` and taking the first occurrence. For each family-level column,
#' order is obtained the same way.
#'
#' The `*_in_lookup` flag on each higher-rank output indicates whether the
#' rolled-up taxon name (e.g., the genus name a species rolls up to) exists
#' in the corresponding lookup column.
#'
#' @importFrom dplyr select distinct bind_rows
#' @importFrom tibble tibble
#' @importFrom rlang .data :=
#' @export
calc_diet_prop <- function(rra, lookup, resolve = c("all", "skip")) {

  resolve <- match.arg(resolve)

  # Coerce input to a numeric matrix with column names preserved.
  if (is.matrix(rra)) {
    mat <- rra
  } else if (is.data.frame(rra)) {
    if (!all(vapply(rra, is.numeric, logical(1)))) {
      stop("All columns of `rra` must be numeric.")
    }
    mat <- as.matrix(rra)
  } else {
    stop("`rra` must be a data.frame, tibble, or numeric matrix.")
  }
  if (!is.numeric(mat)) {
    stop("All columns of `rra` must be numeric.")
  }
  if (is.null(colnames(mat))) {
    stop("`rra` must have column names (taxon identifiers).")
  }

  taxa <- colnames(mat)

  # Check which lookup columns are available; warn and skip missing levels.
  lookup_cols <- names(lookup)
  has_sp <- "Scientific.Name" %in% lookup_cols
  has_gn <- "Genus"           %in% lookup_cols
  has_fm <- "Family"          %in% lookup_cols
  has_od <- "Order"           %in% lookup_cols

  missing_cols <- c(
    if (!has_sp) "Scientific.Name",
    if (!has_gn) "Genus",
    if (!has_fm) "Family",
    if (!has_od) "Order"
  )
  if (length(missing_cols) > 0) {
    warning(
      "lookup is missing column(s): ",
      paste(missing_cols, collapse = ", "),
      ". Affected ranks will be skipped."
    )
  }

  # Unique reference vectors for each rank.
  sp_names <- if (has_sp) unique(lookup$Scientific.Name) else character(0)
  gn_names <- if (has_gn) unique(lookup$Genus)           else character(0)
  fm_names <- if (has_fm) unique(lookup$Family)          else character(0)
  od_names <- if (has_od) unique(lookup$Order)           else character(0)

  # Build dedup'd parent-resolution tables.
  sp_parents <- if (has_sp) {
    sel <- c("Scientific.Name",
             if (has_gn) "Genus",
             if (has_fm) "Family",
             if (has_od) "Order")
    lookup |>
      dplyr::select(dplyr::all_of(sel)) |>
      dplyr::distinct(.data$Scientific.Name, .keep_all = TRUE)
  } else NULL

  gn_parents <- if (has_gn) {
    sel <- c("Genus",
             if (has_fm) "Family",
             if (has_od) "Order")
    lookup |>
      dplyr::select(dplyr::all_of(sel)) |>
      dplyr::distinct(.data$Genus, .keep_all = TRUE)
  } else NULL

  fm_parents <- if (has_fm) {
    sel <- c("Family",
             if (has_od) "Order")
    lookup |>
      dplyr::select(dplyr::all_of(sel)) |>
      dplyr::distinct(.data$Family, .keep_all = TRUE)
  } else NULL

  # Assign each input column to its lowest-resolution rank.
  rank <- vapply(taxa, function(t) {
    if (has_sp && t %in% sp_names) "species"
    else if (has_gn && t %in% gn_names) "genus"
    else if (has_fm && t %in% fm_names) "family"
    else if (has_od && t %in% od_names) "order"
    else NA_character_
  }, character(1))

  unmatched <- taxa[is.na(rank)]

  # For each column and each output level, the higher-rank name it rolls up to.
  taxon_at <- function(level) {
    out <- rep(NA_character_, length(taxa))
    for (i in seq_along(taxa)) {
      t <- taxa[i]
      r <- rank[i]
      if (is.na(r)) next
      if (level == "species") {
        if (r == "species") out[i] <- t
      } else if (level == "genus") {
        if (r == "species" && !is.null(sp_parents) && "Genus" %in% names(sp_parents)) {
          out[i] <- sp_parents$Genus[match(t, sp_parents$Scientific.Name)]
        } else if (r == "genus") {
          out[i] <- t
        }
      } else if (level == "family") {
        if (r == "species" && !is.null(sp_parents) && "Family" %in% names(sp_parents)) {
          out[i] <- sp_parents$Family[match(t, sp_parents$Scientific.Name)]
        } else if (r == "genus" && !is.null(gn_parents) && "Family" %in% names(gn_parents)) {
          out[i] <- gn_parents$Family[match(t, gn_parents$Genus)]
        } else if (r == "family") {
          out[i] <- t
        }
      } else if (level == "order") {
        if (r == "species" && !is.null(sp_parents) && "Order" %in% names(sp_parents)) {
          out[i] <- sp_parents$Order[match(t, sp_parents$Scientific.Name)]
        } else if (r == "genus" && !is.null(gn_parents) && "Order" %in% names(gn_parents)) {
          out[i] <- gn_parents$Order[match(t, gn_parents$Genus)]
        } else if (r == "family" && !is.null(fm_parents) && "Order" %in% names(fm_parents)) {
          out[i] <- fm_parents$Order[match(t, fm_parents$Family)]
        } else if (r == "order") {
          out[i] <- t
        }
      }
    }
    out
  }

  # Sum RRA by group within each sample, then summarize across samples as
  # max + sd capped at 1. Returns a tibble with the rolled-up name and value.
  summarize_level <- function(level, name_col) {
    group_vec <- taxon_at(level)
    keep <- !is.na(group_vec)
    if (!any(keep)) {
      return(tibble::tibble(!!name_col := character(0), diet_proportion = numeric(0)))
    }

    sub <- mat[, keep, drop = FALSE]
    grp <- group_vec[keep]

    unique_grps <- unique(grp)
    summed <- vapply(
      unique_grps,
      function(g) rowSums(sub[, grp == g, drop = FALSE], na.rm = TRUE),
      numeric(nrow(sub))
    )
    if (is.null(dim(summed))) {
      summed <- matrix(summed, nrow = nrow(sub),
                       dimnames = list(NULL, unique_grps))
    } else {
      colnames(summed) <- unique_grps
    }

    stat <- apply(summed, 2, function(v) {
      s <- stats::sd(v, na.rm = TRUE)
      pmin(max(v, na.rm = TRUE) + s, 1)
    })

    tibble::tibble(
      !!name_col      := unique_grps,
      diet_proportion = as.numeric(stat)
    )
  }

  # Add the `*_in_lookup` flag for genus/family/order outputs.
  add_flag <- function(out, name_col, ref_names) {
    flag_col <- paste0(tolower(name_col), "_in_lookup")
    out[[flag_col]] <- out[[name_col]] %in% ref_names
    out
  }

  # Append unmatched columns with diet_proportion = NA_real_ when resolve = "all".
  append_unmatched <- function(out, name_col, flag_col = NULL) {
    if (resolve == "all" && length(unmatched) > 0) {
      um <- tibble::tibble(
        !!name_col      := unmatched,
        diet_proportion = rep(NA_real_, length(unmatched))
      )
      if (!is.null(flag_col)) um[[flag_col]] <- FALSE
      out <- dplyr::bind_rows(out, um)
    }
    out
  }

  # Build each level's output, returning NULL if the lookup column is absent.
  species_out <- if (has_sp) {
    out <- summarize_level("species", "species")
    append_unmatched(out, "species")
  } else NULL

  genus_out <- if (has_gn) {
    out <- summarize_level("genus", "Genus")
    out <- add_flag(out, "Genus", gn_names)
    append_unmatched(out, "Genus", "genus_in_lookup")
  } else NULL

  family_out <- if (has_fm) {
    out <- summarize_level("family", "Family")
    out <- add_flag(out, "Family", fm_names)
    append_unmatched(out, "Family", "family_in_lookup")
  } else NULL

  order_out <- if (has_od) {
    out <- summarize_level("order", "Order")
    out <- add_flag(out, "Order", od_names)
    append_unmatched(out, "Order", "order_in_lookup")
  } else NULL

  list(
    species = species_out,
    genus   = genus_out,
    family  = family_out,
    order   = order_out
  )
}
