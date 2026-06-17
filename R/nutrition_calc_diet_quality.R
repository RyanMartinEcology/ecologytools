#' Weighted Diet Quality of RRA Samples
#'
#' Computes per-sample digestible protein (DP) and digestible energy (DE) for
#' relative-read-abundance (RRA) diet metabarcoding samples, weighting each
#' taxon's stage-specific forage quality by its RRA. For each sample, every
#' taxon is resolved to a taxonomic level via \code{lookup}, assigned a
#' phenological stage from \code{mode} for the sample's seasonal period, and
#' given a DP/DE value from \code{qual} for that stage. The sample score is the
#' RRA-weighted mean over the taxa that yield a usable value, computed
#' independently for DP and DE.
#'
#' Resolution and fall-through:
#' a taxon column name is matched in \code{lookup} as a species (binomial), a
#' genus (the first word of a binomial, or a single-word name), a family, or an
#' order; \code{functional_group} and \code{growth_form} are taken as the modal
#' value across the matched rows. Phenology is assigned in precedence order:
#' MOSS in either group field drops the taxon, TREE forces \code{T}, FERN forces
#' \code{FE}, otherwise \code{mode} is searched from the taxon's level down to
#' \code{all} for the first non-\code{NA} code that period. Quality is read at
#' \code{Part == 'ALL'} starting at the taxon's resolved level and walking up to
#' coarser levels whenever a row is missing or its value is \code{NA}. Taxa not
#' found in \code{lookup}, dropped for MOSS, lacking any phenology code, or
#' lacking a usable quality value are excluded and their weight removed; the
#' surviving weights form each metric's denominator.
#'
#' @param rra A data frame of RRA samples: the columns named in \code{meta_cols}
#'   plus one numeric column per taxon, each row summing to 1.
#' @param qual A list of quality tibbles by level (\code{species}, \code{genus},
#'   \code{family}, \code{functional_group}, \code{growth_form}), each keyed by
#'   its level's name column plus \code{Phenology} and \code{Part}, with
#'   \code{DP_mean} and \code{DE_mean}.
#' @param mode A list of phenology summaries by level (the five above plus
#'   \code{all}), as returned by \code{calc_phenology_mode}: a key column then
#'   one modal-code column per seasonal period.
#' @param lookup A taxonomy crosswalk with columns \code{species}, \code{genus},
#'   \code{family}, \code{order}, \code{functional_group}, \code{growth_form}.
#' @param meta_cols Character vector of non-taxon columns in \code{rra}. Defaults
#'   to \code{c('sample_id', 'animal_id', 'date', 'year', 'UTME', 'UTMN')}.
#' @param tol Numeric tolerance for the per-row sum-to-1 check. Defaults to
#'   \code{1e-6}.
#'
#' @return A list of two data frames. \code{quality} has one row per sample:
#'   the \code{meta_cols}, \code{DP}, \code{DE}, and \code{dp_coverage} /
#'   \code{de_coverage} (the summed RRA actually scored for each metric).
#'   \code{dropped} has one row per dropped taxon-sample: \code{sample_id},
#'   \code{rra_taxon}, \code{rra_value}, \code{entry_level}, and \code{reason}
#'   (\code{unresolved}, \code{moss}, \code{na_phenology}, \code{na_quality_DP},
#'   or \code{na_quality_DE}).
#'
#' @seealso \code{\link{calc_phenology_mode}}
#'
#' @importFrom stats setNames
#' @importFrom utils head
#'
#' @export
calc_diet_quality <- function(rra, qual, mode, lookup,
                              meta_cols = c('sample_id', 'animal_id', 'date', 'year', 'UTME', 'UTMN'),
                              tol = 1e-6) {

  # ----------------------------------------------------------------------------------------------------------------------
  # validate inputs
  # ----------------------------------------------------------------------------------------------------------------------

  #1) require the expected container types and list elements
  qlv <- c('species', 'genus', 'family', 'functional_group', 'growth_form')
  mlv <- c(qlv, 'all')
  llv <- c('species', 'genus', 'family', 'order', 'functional_group', 'growth_form')
  if (!is.data.frame(rra)) {
    stop('`rra` must be a data frame.', call. = F)
  }
  if (!is.list(qual) || !all(qlv %in% names(qual))) {
    stop(
      '`qual` must be a list with elements: ',
      paste(qlv, collapse = ', '),
      '.',
      call. = F
    )
  }
  if (!is.list(mode) || !all(mlv %in% names(mode))) {
    stop(
      '`mode` must be a list with elements: ',
      paste(mlv, collapse = ', '),
      '.',
      call. = F
    )
  }
  if (!is.data.frame(lookup) || !all(llv %in% names(lookup))) {
    stop(
      '`lookup` must be a data frame with columns: ',
      paste(llv, collapse = ', '),
      '.',
      call. = F
    )
  }

  #2) require the metadata columns, leaving at least one taxon column
  miss <- setdiff(x = meta_cols, y = names(rra))
  if (length(miss)) {
    stop(
      '`rra` is missing metadata column(s): ',
      paste(miss, collapse = ', '),
      '. Adjust `meta_cols` or add them.',
      call. = F
    )
  }
  taxa <- setdiff(x = names(rra), y = meta_cols)
  if (!length(taxa)) {
    stop('No taxon columns found in `rra` after removing `meta_cols`.', call. = F)
  }

  #3) require numeric taxon columns
  nonnum <- taxa[!vapply(
    X = rra[taxa],
    FUN = is.numeric,
    FUN.VALUE = logical(1)
  )]
  if (length(nonnum)) {
    stop(
      'These taxon columns are not numeric: ',
      paste(head(x = nonnum, n = 10), collapse = ', '),
      '. Taxon columns must be numeric RRA values.',
      call. = F
    )
  }

  #4) forbid NA taxon cells (absence should be coded 0, not NA)
  M <- as.matrix(rra[taxa])
  if (anyNA(M)) {
    nacol <- taxa[colSums(M_na <- is.na(M)) > 0]
    narow <- which(rowSums(M_na) > 0)
    sid <- if ('sample_id' %in% names(rra)) paste0(' (sample_id ', rra$sample_id[narow[1]], ')') else ''
    stop(
      '`NA` found in taxon cells (absence should be coded 0, not NA). Offending columns: ',
      paste(head(x = nacol, n = 10), collapse = ', '),
      '. First affected row: ',
      narow[1],
      sid,
      '.',
      call. = F
    )
  }

  #5) require non-NA sample dates
  if ('date' %in% meta_cols && anyNA(rra$date)) {
    stop('`rra$date` contains NA; every sample needs a date to map to a period.', call. = F)
  }

  #6) require each sample's taxon weights to sum to 1 within tol
  rs <- rowSums(M)
  bad <- which(abs(rs - 1) > tol)
  if (length(bad)) {
    ids <- if ('sample_id' %in% names(rra)) rra$sample_id[bad] else paste0('row ', bad)
    stop(
      "These samples' RRA values do not sum to 1 (tolerance ",
      tol,
      '): ',
      paste0(
        head(x = ids, n = 5),
        ' (sum ',
        round(x = head(x = rs[bad], n = 5), digits = 4),
        ')',
        collapse = ', '
      ),
      '. ',
      length(bad),
      ' of ',
      nrow(rra),
      ' samples affected.',
      call. = F
    )
  }

  # ----------------------------------------------------------------------------------------------------------------------
  # resolve each taxon to a level and keys
  # ----------------------------------------------------------------------------------------------------------------------

  #1) modal character value, alphabetical tie-break, NA-dropping
  modal_chr <- function(x) {
    x <- x[!is.na(x)]
    if (!length(x)) return(NA_character_)
    tb <- sort(x = table(x), decreasing = T)
    top <- names(tb)[tb == max(tb)]
    sort(top)[1]
  }

  #2) resolve one taxon name: entry level, keys, and MOSS/TREE/FERN flags
  resolve_one <- function(nm) {
    parts <- trimws(strsplit(x = nm, split = '\\s+')[[1]])
    is_binom <- length(parts) >= 2
    gc <- if (is_binom) parts[1] else nm

    if (is_binom && nm %in% lookup$species) {
      rows <- lookup[lookup$species == nm, , drop = F]; entry <- 'species'
    } else if (gc %in% lookup$genus) {
      rows <- lookup[lookup$genus == gc, , drop = F]; entry <- 'genus'
    } else if (!is_binom && nm %in% lookup$family) {
      rows <- lookup[lookup$family == nm, , drop = F]; entry <- 'family'
    } else if (!is_binom && nm %in% lookup$order) {
      rows <- lookup[lookup$order == nm, , drop = F]; entry <- 'order'
    } else {
      return(data.frame(
        taxon = nm,
        entry_level = 'unresolved',
        key_species = NA_character_,
        key_genus = NA_character_,
        key_family = NA_character_,
        key_fg = NA_character_,
        key_gf = NA_character_,
        is_moss = F,
        is_tree = F,
        is_fern = F,
        stringsAsFactors = F
      ))
    }

    fg <- modal_chr(rows$functional_group)
    gf <- modal_chr(rows$growth_form)
    data.frame(
      taxon = nm,
      entry_level = entry,
      key_species = if (entry == 'species') nm else NA_character_,
      key_genus = if (entry %in% c('species', 'genus')) modal_chr(rows$genus) else NA_character_,
      key_family = if (entry %in% c('species', 'genus', 'family')) modal_chr(rows$family) else NA_character_,
      key_fg = fg,
      key_gf = gf,
      is_moss = any(grepl(pattern = 'MOSS', x = c(fg, gf), ignore.case = T)),
      is_tree = any(grepl(pattern = 'TREE', x = c(fg, gf), ignore.case = T)),
      is_fern = any(grepl(pattern = 'FERN', x = c(fg, gf), ignore.case = T)),
      stringsAsFactors = F
    )
  }

  #3) resolve every taxon column once
  res <- do.call(what = rbind, args = lapply(X = taxa, FUN = resolve_one))
  rownames(res) <- res$taxon

  # ----------------------------------------------------------------------------------------------------------------------
  # map each sample date to a period
  # ----------------------------------------------------------------------------------------------------------------------

  #1) read the period labels off mode and parse their date ranges
  keycol_mode <- c(species = 'Species', genus = 'Genus', family = 'Family',
                   functional_group = 'Functional.Group', growth_form = 'Growth.Form', all = 'All')
  period_cols <- setdiff(x = names(mode$all), y = keycol_mode['all'])
  pr <- strsplit(x = period_cols, split = '-')
  starts <- trimws(vapply(X = pr, FUN = `[`, FUN.VALUE = '', 1))
  ends <- trimws(vapply(X = pr, FUN = `[`, FUN.VALUE = '', 2))
  p_start <- as.Date(x = paste('2001', starts), format = '%Y %b %d')
  p_end <- as.Date(x = paste('2001', ends), format = '%Y %b %d')

  #2) order windows chronologically
  o <- order(p_start)
  period_cols <- period_cols[o]
  p_start <- p_start[o]
  p_end <- p_end[o]

  #3) assign each date to its window, clamping out-of-range dates to the nearest
  assign_period <- function(dates) {
    d <- as.Date(x = paste0('2001-', format(x = dates, format = '%m-%d')))
    idx <- rep(x = NA_integer_, times = length(d))
    for (i in seq_along(period_cols)) {
      hit <- is.na(idx) & !is.na(d) & d >= p_start[i] & d <= p_end[i]
      idx[hit] <- i
    }
    idx[is.na(idx) & !is.na(d) & d < p_start[1]] <- 1L
    idx[is.na(idx) & !is.na(d) & d > p_end[length(p_end)]] <- length(period_cols)
    period_cols[idx]
  }
  periods <- assign_period(rra$date)

  # ----------------------------------------------------------------------------------------------------------------------
  # build indexed mode and quality lookups
  # ----------------------------------------------------------------------------------------------------------------------

  #1) level order and where each entry level starts in it
  level_seq <- c('species', 'genus', 'family', 'functional_group', 'growth_form', 'all')
  entry_idx <- c(species = 1L, genus = 2L, family = 3L, order = 4L)
  key_for_level <- function(r, lvl) {
    switch(lvl,
           species = r$key_species,
           genus = r$key_genus,
           family = r$key_family,
           functional_group = r$key_fg,
           growth_form = r$key_gf,
           all = 'All')
  }

  #2) name -> row index within each mode table
  mode_idx <- lapply(X = level_seq, FUN = function(lvl) {
    tb <- mode[[lvl]]
    setNames(object = seq_len(nrow(tb)), nm = tb[[keycol_mode[lvl]]])
  })
  names(mode_idx) <- level_seq

  #3) Part == 'ALL' slice of each quality table
  keycol_qual <- c(species = 'Scientific.Name', genus = 'Genus', family = 'Family',
                   functional_group = 'Functional.Group', growth_form = 'Growth.Form')
  qual_levels <- c('species', 'genus', 'family', 'functional_group', 'growth_form')
  qual_all <- lapply(X = qual_levels, FUN = function(lvl) {
    tb <- qual[[lvl]]
    tb[tb$Part == 'ALL', , drop = F]
  })
  names(qual_all) <- qual_levels

  #4) phenology code for a taxon in a period, via the fall-through chain
  phen_for <- function(r, period) {
    if (r$entry_level == 'unresolved' || isTRUE(r$is_moss)) return(NA_character_)
    if (isTRUE(r$is_tree)) return('T')
    if (isTRUE(r$is_fern)) return('FE')
    for (k in entry_idx[[r$entry_level]]:length(level_seq)) {
      lvl <- level_seq[k]
      key <- key_for_level(r, lvl)
      if (is.na(key)) next
      ix <- mode_idx[[lvl]][key]
      if (is.na(ix)) next
      code <- mode[[lvl]][[period]][ix]
      if (!is.na(code)) return(code)
    }
    NA_character_
  }

  #5) DP/DE for a taxon at a phenology code, walking up from its entry level
  qual_for <- function(r, code, metric) {
    if (is.na(code) || !(r$entry_level %in% names(entry_idx))) return(NA_real_)
    for (k in entry_idx[[r$entry_level]]:length(qual_levels)) {
      lvl <- qual_levels[k]
      key <- key_for_level(r, lvl)
      if (is.na(key)) next
      tb <- qual_all[[lvl]]
      hit <- tb[[keycol_qual[lvl]]] == key & tb$Phenology == code
      v <- tb[[metric]][hit]
      v <- v[!is.na(v) & !is.nan(v)]
      if (length(v)) return(mean(v))
    }
    NA_real_
  }

  # ----------------------------------------------------------------------------------------------------------------------
  # precompute phenology by period and quality by code
  # ----------------------------------------------------------------------------------------------------------------------

  #1) every taxon x period phenology code, and every taxon x code DP/DE
  codes_all <- c('N/B', 'FL', 'FR', 'M', 'C', 'T', 'FE')
  phen_tab <- matrix(
    data = NA_character_,
    nrow = nrow(res),
    ncol = length(period_cols),
    dimnames = list(res$taxon, period_cols)
  )
  DPq <- matrix(
    data = NA_real_,
    nrow = nrow(res),
    ncol = length(codes_all),
    dimnames = list(res$taxon, codes_all)
  )
  DEq <- DPq
  for (j in seq_len(nrow(res))) {
    r <- res[j, ]
    for (p in period_cols) phen_tab[j, p] <- phen_for(r, p)
    for (cc in codes_all) {
      DPq[j, cc] <- qual_for(r, cc, 'DP_mean')
      DEq[j, cc] <- qual_for(r, cc, 'DE_mean')
    }
  }

  # ----------------------------------------------------------------------------------------------------------------------
  # aggregate weighted quality per sample
  # ----------------------------------------------------------------------------------------------------------------------

  #1) per-taxon flags and sample ids used in the loop
  res_unres <- setNames(object = res$entry_level == 'unresolved', nm = res$taxon)
  res_moss <- setNames(object = res$is_moss %in% T, nm = res$taxon)
  sid_all <- if ('sample_id' %in% names(rra)) rra$sample_id else seq_len(nrow(rra))

  #2) walk each sample's taxa, accumulating survivor-weighted DP/DE and drops
  DP <- rep(x = NA_real_, times = nrow(rra))
  DE <- DP
  dpc <- DP
  dec <- DP
  drop_list <- vector(mode = 'list', length = nrow(rra))

  for (i in seq_len(nrow(rra))) {
    per <- periods[i]
    w <- M[i, ]
    nz <- names(w)[w > 0]
    dp_num <- 0; dp_den <- 0; de_num <- 0; de_den <- 0
    d_tax <- character(0); d_val <- numeric(0); d_lvl <- character(0); d_rsn <- character(0)

    for (t in nz) {
      wv <- w[[t]]
      reason <- NA_character_
      code <- NA_character_
      dp <- NA_real_
      de <- NA_real_

      if (res_unres[[t]]) {
        reason <- 'unresolved'
      } else if (res_moss[[t]]) {
        reason <- 'moss'
      } else {
        code <- phen_tab[t, per]
        if (is.na(code)) {
          reason <- 'na_phenology'
        } else if (code %in% codes_all) {
          dp <- DPq[t, code]
          de <- DEq[t, code]
        }
      }

      if (!is.na(dp)) { dp_num <- dp_num + wv * dp; dp_den <- dp_den + wv }
      if (!is.na(de)) { de_num <- de_num + wv * de; de_den <- de_den + wv }

      if (!is.na(reason)) {
        d_tax <- c(d_tax, t); d_val <- c(d_val, wv)
        d_lvl <- c(d_lvl, res[t, 'entry_level']); d_rsn <- c(d_rsn, reason)
      } else {
        if (is.na(dp)) {
          d_tax <- c(d_tax, t); d_val <- c(d_val, wv)
          d_lvl <- c(d_lvl, res[t, 'entry_level']); d_rsn <- c(d_rsn, 'na_quality_DP')
        }
        if (is.na(de)) {
          d_tax <- c(d_tax, t); d_val <- c(d_val, wv)
          d_lvl <- c(d_lvl, res[t, 'entry_level']); d_rsn <- c(d_rsn, 'na_quality_DE')
        }
      }
    }

    DP[i] <- if (dp_den > 0) dp_num / dp_den else NA_real_
    DE[i] <- if (de_den > 0) de_num / de_den else NA_real_
    dpc[i] <- dp_den
    dec[i] <- de_den
    if (length(d_tax)) {
      drop_list[[i]] <- data.frame(
        sample_id = sid_all[i],
        rra_taxon = d_tax,
        rra_value = as.numeric(d_val),
        entry_level = d_lvl,
        reason = d_rsn,
        stringsAsFactors = F
      )
    }
  }

  # ----------------------------------------------------------------------------------------------------------------------
  # assemble output
  # ----------------------------------------------------------------------------------------------------------------------

  #1) per-sample quality table plus the dropped-taxa audit
  quality <- data.frame(
    rra[meta_cols],
    DP = DP,
    DE = DE,
    dp_coverage = dpc,
    de_coverage = dec,
    stringsAsFactors = F,
    check.names = F
  )
  dropped <- do.call(what = rbind, args = drop_list)
  if (is.null(dropped)) {
    dropped <- data.frame(
      sample_id = character(0),
      rra_taxon = character(0),
      rra_value = numeric(0),
      entry_level = character(0),
      reason = character(0),
      stringsAsFactors = F
    )
  }
  rownames(dropped) <- NULL

  list(quality = quality, dropped = dropped)
}
