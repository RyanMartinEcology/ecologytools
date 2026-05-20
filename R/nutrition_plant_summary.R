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
