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
#' `max + sd`, capped at 1, and returned as a single `diet_proportion`
#' column for each rank. output intended as capped diet proportions for the FRESH model.
#'
#' @param rra A data frame, tibble, or numeric matrix with samples as rows
#'   and taxa as columns. Column names are taxon identifiers (e.g.,
#'   `"Abies lasiocarpa"`, `"Pinaceae"`). All columns must be numeric, and
#'   each row is assumed to be already normalized (e.g., to sum to 1).
#' @param plant_summary Output of [plant_quality_summary()], used as the
#'   taxonomic lookup. Its `$species`, `$genus`, `$family`, and `$order`
#'   tibbles supply the valid `Scientific.Name`, `Genus`, `Family`, and
#'   `Order` values.
#' @param resolve Either `"all"` (default) or `"skip"`. When `"all"`, columns
#'   that cannot be resolved to any rank appear in each level's output with
#'   `diet_proportion = NA_real_`. When `"skip"`, unresolved columns are
#'   silently dropped.
#'
#' @return A named list with elements `species`, `genus`, `family`, `order`.
#'   Each is a tibble with two columns: the taxon name (`species`, `Genus`,
#'   `Family`, or `Order`) and `diet_proportion`.
#'
#' @details
#' Each input column is assigned to its lowest-resolution rank by matching
#' against the lookup tables in `plant_summary`: first
#' `species$Scientific.Name`, then `genus$Genus`, `family$Family`, and
#' `order$Order`. Columns matched at species level contribute to species,
#' genus, family, and order summaries; columns matched at genus level
#' contribute to genus, family, and order; family-level columns contribute
#' to family and order; order-level columns contribute only to order. Genus,
#' family, and order are looked up for each species via the lookup tables.
#'
#' @importFrom dplyr group_by summarise mutate select left_join distinct bind_rows
#' @importFrom tibble tibble as_tibble
#' @importFrom rlang .data :=
#' @export
calc_diet_prop <- function(rra, plant_summary, resolve = c("all", "skip")) {

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

  # Pull lookup vectors from plant_summary; tolerate missing levels.
  sp_lookup <- if (!is.null(plant_summary$species)) plant_summary$species else NULL
  gn_lookup <- if (!is.null(plant_summary$genus))   plant_summary$genus   else NULL
  fm_lookup <- if (!is.null(plant_summary$family))  plant_summary$family  else NULL
  od_lookup <- if (!is.null(plant_summary$order))   plant_summary$order   else NULL

  sp_names <- if (!is.null(sp_lookup)) unique(sp_lookup$Scientific.Name) else character(0)
  gn_names <- if (!is.null(gn_lookup)) unique(gn_lookup$Genus)           else character(0)
  fm_names <- if (!is.null(fm_lookup)) unique(fm_lookup$Family)          else character(0)
  od_names <- if (!is.null(od_lookup)) unique(od_lookup$Order)           else character(0)

  # Assign each column to its lowest-resolution rank.
  rank <- vapply(taxa, function(t) {
    if (t %in% sp_names) "species"
    else if (t %in% gn_names) "genus"
    else if (t %in% fm_names) "family"
    else if (t %in% od_names) "order"
    else NA_character_
  }, character(1))

  unmatched <- taxa[is.na(rank)]

  # Resolve each species-level column to its genus, family, and order via the
  # species lookup table.
  sp_to_higher <- if (!is.null(sp_lookup)) {
    sp_lookup |>
      dplyr::select(.data$Scientific.Name, .data$Genus, .data$Family, .data$Order) |>
      dplyr::distinct(.data$Scientific.Name, .keep_all = TRUE)
  } else {
    tibble::tibble(
      Scientific.Name = character(0),
      Genus = character(0), Family = character(0), Order = character(0)
    )
  }

  # Resolve each genus-level column to its family and order via the genus
  # lookup table.
  gn_to_higher <- if (!is.null(gn_lookup)) {
    gn_lookup |>
      dplyr::select(.data$Genus, .data$Family, .data$Order) |>
      dplyr::distinct(.data$Genus, .keep_all = TRUE)
  } else {
    tibble::tibble(Genus = character(0), Family = character(0), Order = character(0))
  }

  # Resolve each family-level column to its order via the family lookup table.
  fm_to_higher <- if (!is.null(fm_lookup)) {
    fm_lookup |>
      dplyr::select(.data$Family, .data$Order) |>
      dplyr::distinct(.data$Family, .keep_all = TRUE)
  } else {
    tibble::tibble(Family = character(0), Order = character(0))
  }

  # For a given output level, return the higher-rank name each input column
  # rolls up to (NA if the column does not contribute at that level).
  taxon_at <- function(level) {
    out <- rep(NA_character_, length(taxa))
    for (i in seq_along(taxa)) {
      t <- taxa[i]
      r <- rank[i]
      if (is.na(r)) next
      if (level == "species") {
        if (r == "species") out[i] <- t
      } else if (level == "genus") {
        if (r == "species") {
          out[i] <- sp_to_higher$Genus[match(t, sp_to_higher$Scientific.Name)]
        } else if (r == "genus") {
          out[i] <- t
        }
      } else if (level == "family") {
        if (r == "species") {
          g <- sp_to_higher$Genus[match(t, sp_to_higher$Scientific.Name)]
          out[i] <- gn_to_higher$Family[match(g, gn_to_higher$Genus)]
        } else if (r == "genus") {
          out[i] <- gn_to_higher$Family[match(t, gn_to_higher$Genus)]
        } else if (r == "family") {
          out[i] <- t
        }
      } else if (level == "order") {
        if (r == "species") {
          g <- sp_to_higher$Genus[match(t, sp_to_higher$Scientific.Name)]
          out[i] <- gn_to_higher$Order[match(g, gn_to_higher$Genus)]
        } else if (r == "genus") {
          out[i] <- gn_to_higher$Order[match(t, gn_to_higher$Genus)]
        } else if (r == "family") {
          out[i] <- fm_to_higher$Order[match(t, fm_to_higher$Family)]
        } else if (r == "order") {
          out[i] <- t
        }
      }
    }
    out
  }

  # Append unmatched columns to a level's output with diet_proportion = NA_real_.
  append_unmatched <- function(out, name_col) {
    if (resolve == "all" && length(unmatched) > 0) {
      um <- tibble::tibble(
        !!name_col      := unmatched,
        diet_proportion = rep(NA_real_, length(unmatched))
      )
      out <- dplyr::bind_rows(out, um)
    }
    out
  }

  # Sum sample-level RRA by taxon group at a given rank, then summarize across
  # samples as max + sd capped at 1.
  summarize_level <- function(level, name_col) {
    group_vec <- taxon_at(level)
    keep <- !is.na(group_vec)
    if (!any(keep)) {
      out <- tibble::tibble(!!name_col := character(0), diet_proportion = numeric(0))
      return(append_unmatched(out, name_col))
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

    out <- tibble::tibble(
      !!name_col      := unique_grps,
      diet_proportion = as.numeric(stat)
    )
    append_unmatched(out, name_col)
  }

  list(
    species = summarize_level("species", "species"),
    genus   = summarize_level("genus",   "Genus"),
    family  = summarize_level("family",  "Family"),
    order   = summarize_level("order",   "Order")
  )
}
