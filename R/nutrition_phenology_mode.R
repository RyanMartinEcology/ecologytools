#' Modal Phenology by Group per Month
#'
#' Summarizes the modal (most frequent) phenology code for each group in each
#' month present in the data. When two or more phenology codes are tied for
#' most frequent within a group-month, all tied codes are returned as a
#' comma-separated string. Group-month combinations with no observations are
#' returned as \code{NA}.
#'
#' @param data A data frame containing at least the columns named by
#'   \code{group}, \code{phenology}, and \code{date}.
#' @param group Character. Name of the grouping column (e.g., \code{"Genus"}
#'   or \code{"Family"}).
#' @param phenology Character. Name of the phenology column. Values are
#'   treated as character codes.
#' @param date Character. Name of a \code{POSIXct} (or \code{Date}) column
#'   from which the month will be extracted.
#'
#' @return A data frame with one row per group level and one column per month
#'   present in the data (named by three-letter month abbreviation, ordered
#'   Jan-Dec). Cells contain the modal phenology code(s) for that group-month,
#'   or \code{NA} if no observations exist.
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   Genus = sample(c("Vaccinium", "Chimaphila", "Amelanchier"), 50, replace = TRUE),
#'   Phenology = sample(c("N/B", "FR", "FL"), 50, replace = TRUE),
#'   Date = as.POSIXct("2023-07-01") + sample(0:120, 50, replace = TRUE) * 86400
#' )
#' modal_phenology_by_month(df, "Genus", "Phenology", "Date")
#'
#' @export
modal_phenology_by_month <- function(data, group, phenology, date) {

  stopifnot(
    is.data.frame(data),
    is.character(group), length(group) == 1L,
    is.character(phenology), length(phenology) == 1L,
    is.character(date), length(date) == 1L,
    all(c(group, phenology, date) %in% names(data))
  )

  grp <- data[[group]]
  phen <- as.character(data[[phenology]])
  dt <- data[[date]]

  if (!inherits(dt, c("POSIXct", "POSIXt", "Date"))) {
    stop("`", date, "` must be a POSIXct, POSIXt, or Date column.")
  }

  month_abb <- format(dt, "%b")
  present <- month.abb[month.abb %in% unique(month_abb)]
  month_fac <- factor(month_abb, levels = present)

  modal_phen <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0L) return(NA_character_)
    tab <- table(x)
    modes <- names(tab)[tab == max(tab)]
    paste(modes, collapse = ", ")
  }

  work <- data.frame(
    .group = grp,
    .month = month_fac,
    .phen = phen,
    stringsAsFactors = FALSE
  )

  agg <- aggregate(
    .phen ~ .group + .month,
    data = work,
    FUN = modal_phen,
    drop = FALSE
  )

  out <- reshape(
    agg,
    idvar = ".group",
    timevar = ".month",
    direction = "wide"
  )

  names(out) <- sub("^\\.phen\\.", "", names(out))
  names(out)[names(out) == ".group"] <- group
  rownames(out) <- NULL
  out
}
