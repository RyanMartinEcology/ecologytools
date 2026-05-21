#' Summarize plant forage quality at species, genus, family, functional group,
#' and growth form levels
#'
#' Computes mean, standard deviation, and non-NA sample size for digestible
#' energy (DE), digestible protein (DP), and dry matter digestibility (DMD)
#' at up to five resolutions. Higher-level summaries are computed directly
#' from the raw input rows. Where a higher-level grouping spans multiple
#' parent values, the first occurrence is retained.
#'
#' Grouping levels (`Code`/`Scientific.Name`, `Genus`, `Family`,
#' `Functional.Group`, `Growth.Form`) and optional grouping columns
#' (`Phenology`, `Part`) are used only if present in `data`. At least one of
#' `Code`, `Genus`, `Family`, `Functional.Group`, or `Growth.Form` must be
#' present. Levels not represented in the data are returned as `NULL`.
#'
#' @param data A data frame containing `DE`, `DP`, `DMD`, and at least one of
#'   `Code`, `Genus`, `Family`, `Functional.Group`, or `Growth.Form`. May
#'   optionally contain `Scientific.Name`, `Phenology`, and `Part`.
#'
#' @return A named list with elements `species`, `genus`, `family`,
#'   `functional_group`, `growth_form`. Each is either a tibble of summaries
#'   or `NULL` if the corresponding column was absent from `data`.
#'
#' @importFrom dplyr group_by summarise left_join select distinct everything all_of across
#' @importFrom rlang .data
#' @export
plant_quality_summary <- function(data) {
  cols <- names(data)
  # Identify which levels and optional groupers are available.
  has_species    <- "Code"             %in% cols
  has_genus      <- "Genus"            %in% cols
  has_family     <- "Family"           %in% cols
  has_funcgroup  <- "Functional.Group" %in% cols
  has_growthform <- "Growth.Form"      %in% cols
  if (!any(has_species, has_genus, has_family, has_funcgroup, has_growthform)) {
    stop("`data` must contain at least one of: Code, Genus, Family, Functional.Group, Growth.Form.")
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
  # Species level: carry Scientific.Name, Genus, Family, Functional.Group, Growth.Form.
  species_summary <- if (has_species) {
    summarize_level("Code", c("Scientific.Name", "Genus", "Family",
                              "Functional.Group", "Growth.Form"))
  } else NULL
  # Genus level: carry Family, Functional.Group, Growth.Form.
  genus_summary <- if (has_genus) {
    summarize_level("Genus", c("Family", "Functional.Group", "Growth.Form"))
  } else NULL
  # Family level: carry Functional.Group, Growth.Form.
  family_summary <- if (has_family) {
    summarize_level("Family", c("Functional.Group", "Growth.Form"))
  } else NULL
  # Functional group level: carry Growth.Form.
  funcgroup_summary <- if (has_funcgroup) {
    summarize_level("Functional.Group", "Growth.Form")
  } else NULL
  # Growth form level: nothing to carry.
  growthform_summary <- if (has_growthform) {
    summarize_level("Growth.Form", character(0))
  } else NULL
  list(
    species          = species_summary,
    genus            = genus_summary,
    family           = family_summary,
    functional_group = funcgroup_summary,
    growth_form      = growthform_summary
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
#' Each input column is assigned to a rank as follows:
#' \itemize{
#'   \item Species: exact match against `lookup$Scientific.Name`.
#'   \item Genus: a two-word column name is parsed to its first whitespace-
#'     separated token and treated as a genus; a single-word column name is
#'     used as is. In both cases, the resulting genus contributes to the
#'     genus, family, and order outputs. The `genus_in_lookup` flag indicates
#'     whether that genus appears in `lookup$Genus`.
#'   \item Family: single-word columns that did not match at genus level are
#'     checked against `lookup$Family`.
#'   \item Order: single-word columns that did not match at genus or family
#'     level are checked against `lookup$Order`.
#' }
#'
#' Higher-rank rollups for two-word columns are obtained by looking up the
#' first-token genus in a deduplicated `Genus` → `Family`/`Order` table
#' (first occurrence retained). Genus-rank columns roll up the same way;
#' family-rank columns roll up to order via `lookup$Family` →
#' `lookup$Order` (first occurrence retained). Two-word columns whose first
#' token is not in `lookup$Genus` will receive `NA` for family/order
#' rollups and contribute only at genus level (with `genus_in_lookup =
#' FALSE`).
#'
#' @importFrom dplyr select distinct bind_rows all_of
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

  # Reference vectors for each rank.
  sp_names <- if (has_sp) unique(lookup$Scientific.Name) else character(0)
  gn_names <- if (has_gn) unique(lookup$Genus)           else character(0)
  fm_names <- if (has_fm) unique(lookup$Family)          else character(0)
  od_names <- if (has_od) unique(lookup$Order)           else character(0)

  # Dedup'd parent-resolution tables.
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

  # Per-column metadata.
  n_tokens  <- lengths(strsplit(taxa, "\\s+"))
  first_tok <- vapply(taxa, function(t) strsplit(t, "\\s+")[[1]][1], character(1))

  # Rank assignment:
  #   - Two-word names that match Scientific.Name -> species.
  #   - All other two-word names -> genus (parsed via first token).
  #   - Single-word names: try Genus, then Family, then Order.
  rank <- vapply(seq_along(taxa), function(i) {
    t <- taxa[i]
    if (n_tokens[i] == 2) {
      if (has_sp && t %in% sp_names) return("species")
      if (has_gn) return("genus")
      return(NA_character_)
    }
    if (has_gn && t %in% gn_names) return("genus")
    if (has_fm && t %in% fm_names) return("family")
    if (has_od && t %in% od_names) return("order")
    NA_character_
  }, character(1))

  unmatched <- taxa[is.na(rank)]

  # The genus name a column rolls up under (two-word: first token; one-word:
  # the column itself if it was assigned at genus rank).
  genus_token <- ifelse(rank == "species" | rank == "genus",
                        ifelse(n_tokens == 2, first_tok, taxa),
                        NA_character_)

  # For each column and each output level, return the higher-rank name it
  # rolls up to (NA if it does not contribute at that level).
  taxon_at <- function(level) {
    out <- rep(NA_character_, length(taxa))
    for (i in seq_along(taxa)) {
      r <- rank[i]
      if (is.na(r)) next
      t <- taxa[i]
      if (level == "species") {
        if (r == "species") out[i] <- t
      } else if (level == "genus") {
        if (r == "species" || r == "genus") out[i] <- genus_token[i]
      } else if (level == "family") {
        if (r == "species" || r == "genus") {
          if (!is.null(gn_parents) && "Family" %in% names(gn_parents)) {
            out[i] <- gn_parents$Family[match(genus_token[i], gn_parents$Genus)]
          }
        } else if (r == "family") {
          out[i] <- t
        }
      } else if (level == "order") {
        if (r == "species" || r == "genus") {
          if (!is.null(gn_parents) && "Order" %in% names(gn_parents)) {
            out[i] <- gn_parents$Order[match(genus_token[i], gn_parents$Genus)]
          }
        } else if (r == "family") {
          if (!is.null(fm_parents) && "Order" %in% names(fm_parents)) {
            out[i] <- fm_parents$Order[match(t, fm_parents$Family)]
          }
        } else if (r == "order") {
          out[i] <- t
        }
      }
    }
    out
  }

  # Sum RRA by group within each sample, then summarize across samples as
  # max + sd capped at 1.
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

  add_flag <- function(out, name_col, ref_names) {
    flag_col <- paste0(tolower(name_col), "_in_lookup")
    out[[flag_col]] <- out[[name_col]] %in% ref_names
    out
  }

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
