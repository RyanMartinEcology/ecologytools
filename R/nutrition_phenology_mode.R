#' Modal Phenology by Period Across Grouping Levels
#'
#' Bins vegetation observations into fixed-width calendar periods anchored at
#' the month-day of \code{starting_date} (default July 1) and computes the
#' modal \code{Phenology} code within each period for several grouping levels.
#' Periods are defined by the number of days since the most recent anchor date
#' on or before each observation, so the same seasonal window is pooled across
#' all years present in the data (e.g., the 'Jul 01-Jul 14' cell combines that
#' window from every year).
#'
#' Within each group-period, the mode is the most frequent phenology code,
#' computed only when at least three non-missing observations are present
#' (otherwise the cell is \code{NA}). Ties are broken by the earliest
#' phenological stage in the fixed order \code{N/B < FL < FR < M < C < T < FE}.
#' \code{NA} phenology values are ignored when counting and when computing the
#' mode; they do not trigger an error. Any non-\code{NA} value outside the seven
#' allowed codes causes an error before any computation is done.
#'
#' @param dat A data frame (or tibble) containing at least the columns
#'   \code{Date} (a \code{POSIXct} column), \code{Phenology}, \code{Species},
#'   \code{Genus}, \code{Family}, \code{Functional.Group}, and
#'   \code{Growth.Form}.
#' @param starting_date A length-one \code{POSIXct} giving the seasonal anchor.
#'   Only its month and day are used; the year is ignored so the anchor
#'   generalizes to every year in \code{dat}. Defaults to July 1.
#' @param length Integer. Width of each period in calendar days. Defaults to 14.
#'
#' @return A named list of six data frames. The first five
#'   (\code{Species}, \code{Genus}, \code{Family}, \code{Functional.Group},
#'   \code{Growth.Form}) each have one row per level of that grouping column and
#'   one column per period; the sixth (\code{All}) has a single row summarizing
#'   all observations. Period columns are labelled by their date range
#'   (e.g., \code{'Jul 01-Jul 14'}) and ordered from the anchor through the last
#'   period containing observations, with no gaps. Cells contain the modal
#'   phenology code, or \code{NA} where fewer than three observations exist.
#'
#' @importFrom stats aggregate reshape
#'
#' @examples
#' set.seed(1)
#' dat <- data.frame(
#'   Date = as.POSIXct(x = '2022-07-01', tz = 'UTC') +
#'     sample(
#'       x = 0:430,
#'       size = 200,
#'       replace = T
#'     ) * 86400,
#'   Species.Cover.Class = NA_character_,
#'   Phenology = sample(
#'     x = c(
#'       'N/B',
#'       'FL',
#'       'FR',
#'       'M',
#'       NA
#'     ),
#'     size = 200,
#'     replace = T
#'   ),
#'   Species = sample(x = c('Vaccinium membranaceum', 'Chimaphila umbellata'), size = 200, replace = T),
#'   Genus = sample(x = c('Vaccinium', 'Chimaphila'), size = 200, replace = T),
#'   Family = sample(x = c('Ericaceae', 'Rosaceae'), size = 200, replace = T),
#'   Functional.Group = sample(x = c('EVERGREEN SHRUB', 'DECIDUOUS SHRUB'), size = 200, replace = T),
#'   Growth.Form = 'SHRUB',
#'   stringsAsFactors = F
#' )
#' res <- calc_phenology_mode(dat = dat, length = 14)
#' res$Genus
#' res$All
#'
#' @export
calc_phenology_mode <- function(dat,
                                starting_date = as.POSIXct(x = '2000-07-01', tz = 'UTC'),
                                length = 14) {

  # ----------------------------------------------------------------------------------------------------------------------
  # validate inputs
  # ----------------------------------------------------------------------------------------------------------------------

  #1) require a data frame
  if (!is.data.frame(dat)) {
    stop('`dat` must be a data frame.', call. = F)
  }

  #2) require the expected columns
  req <- c(
    'Date',
    'Phenology',
    'Species',
    'Genus',
    'Family',
    'Functional.Group',
    'Growth.Form'
  )
  miss <- setdiff(x = req, y = names(dat))
  if (length(miss) > 0L) {
    stop(
      '`dat` is missing required column(s): ',
      paste(miss, collapse = ', '),
      '.',
      call. = F
    )
  }

  #3) require a POSIXct date column
  if (!inherits(x = dat$Date, what = c('POSIXct', 'POSIXt'))) {
    stop('`dat$Date` must be a POSIXct column.', call. = F)
  }

  #4) require a length-one POSIXct anchor
  if (!inherits(x = starting_date, what = c('POSIXct', 'POSIXt')) ||
      length(starting_date) != 1L) {
    stop('`starting_date` must be a length-one POSIXct.', call. = F)
  }

  #5) require a positive whole-day period length
  if (!is.numeric(length) || length(length) != 1L || is.na(length) ||
      length < 1 || length != as.integer(length)) {
    stop('`length` must be a single positive whole number of days.', call. = F)
  }
  length <- as.integer(length)

  # ----------------------------------------------------------------------------------------------------------------------
  # validate phenology codes
  # ----------------------------------------------------------------------------------------------------------------------

  #1) define the ordered set of allowed codes
  pheno_levels <- c(
    'N/B',
    'FL',
    'FR',
    'M',
    'C',
    'T',
    'FE'
  )

  #2) error on any non-NA value outside the allowed set
  ok <- is.na(dat$Phenology) | dat$Phenology %in% pheno_levels
  if (!all(ok)) {
    bad <- unique(dat$Phenology[!ok])
    stop(
      '`Phenology` contains values outside the allowed codes: ',
      paste(bad, collapse = ', '),
      '. Allowed codes are: ',
      paste(pheno_levels, collapse = ', '),
      '.',
      call. = F
    )
  }

  #3) build a rank lookup for tie-breaking by earliest stage
  pheno_rank <- seq_along(pheno_levels)
  names(pheno_rank) <- pheno_levels

  # ----------------------------------------------------------------------------------------------------------------------
  # assign observations to pooled periods
  # ----------------------------------------------------------------------------------------------------------------------

  #1) collapse timestamps to calendar dates in the column time zone
  tzc <- attr(x = dat$Date, which = 'tzone')
  if (is.null(tzc) || !nzchar(tzc)) tzc <- 'UTC'
  d_cal <- as.Date(format(
    x = dat$Date,
    format = '%Y-%m-%d',
    tz = tzc
  ))

  #2) read the anchor month and day from starting_date
  am <- as.integer(format(x = starting_date, format = '%m'))
  ad <- as.integer(format(x = starting_date, format = '%d'))

  #3) find the most recent anchor on or before each date
  yr <- as.integer(format(x = d_cal, format = '%Y'))
  anchor <- as.Date(sprintf(
    '%04d-%02d-%02d',
    yr,
    am,
    ad
  ))
  before <- !is.na(anchor) & d_cal < anchor
  anchor[before] <- as.Date(sprintf(
    '%04d-%02d-%02d',
    yr[before] - 1L,
    am,
    ad
  ))

  #4) convert offsets to a 0-based period index
  offset <- as.integer(d_cal - anchor)
  period_idx <- offset %/% length
  if (all(is.na(period_idx))) {
    stop('No usable dates in `dat$Date`.', call. = F)
  }
  all_idx <- 0:max(period_idx, na.rm = T)

  # ----------------------------------------------------------------------------------------------------------------------
  # label periods by date range
  # ----------------------------------------------------------------------------------------------------------------------

  #1) build nominal start and end dates from a fixed non-leap reference
  ref_anchor <- as.Date(sprintf(
    '2001-%02d-%02d',
    am,
    ad
  ))
  lab_start <- ref_anchor + all_idx * length
  lab_end <- ref_anchor + all_idx * length + (length - 1L)

  #2) format the range labels
  period_labels <- paste0(
    format(x = lab_start, format = '%b %d'),
    '-',
    format(x = lab_end, format = '%b %d')
  )

  # ----------------------------------------------------------------------------------------------------------------------
  # define the mode helper
  # ----------------------------------------------------------------------------------------------------------------------

  #1) return the modal code, requiring at least three observations
  modal_pheno <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) < 3L) return(NA_character_)
    tab <- table(x)
    top <- names(tab)[tab == max(tab)]
    top[which.min(pheno_rank[top])]
  }

  # ----------------------------------------------------------------------------------------------------------------------
  # build one wide summary per grouping vector
  # ----------------------------------------------------------------------------------------------------------------------

  #1) aggregate, reshape to wide, and relabel the period columns
  build_summary <- function(g, gname) {
    work <- data.frame(
      .g = g,
      .p = factor(x = period_idx, levels = all_idx),
      .ph = dat$Phenology,
      stringsAsFactors = F
    )
    agg <- aggregate(
      x = .ph ~ .g + .p,
      data = work,
      FUN = modal_pheno,
      drop = F
    )
    wide <- reshape(
      data = agg,
      idvar = '.g',
      timevar = '.p',
      direction = 'wide'
    )
    nm <- names(wide)
    is_p <- grepl(pattern = '^\\.ph\\.', x = nm)
    idx <- as.integer(sub(
      pattern = '^\\.ph\\.',
      replacement = '',
      x = nm[is_p]
    ))
    names(wide)[is_p] <- period_labels[match(x = idx, table = all_idx)]
    names(wide)[names(wide) == '.g'] <- gname
    wide <- wide[, c(gname, period_labels), drop = F]
    rownames(wide) <- NULL
    wide
  }

  # ----------------------------------------------------------------------------------------------------------------------
  # assemble the six summaries
  # ----------------------------------------------------------------------------------------------------------------------

  #1) one summary per grouping column plus an overall summary
  list(
    Species = build_summary(g = dat$Species, gname = 'Species'),
    Genus = build_summary(g = dat$Genus, gname = 'Genus'),
    Family = build_summary(g = dat$Family, gname = 'Family'),
    Functional.Group = build_summary(g = dat$Functional.Group, gname = 'Functional.Group'),
    Growth.Form = build_summary(g = dat$Growth.Form, gname = 'Growth.Form'),
    All = build_summary(g = rep(x = 'All', times = nrow(dat)), gname = 'All')
  )
}
