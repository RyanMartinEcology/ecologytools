# Modal Phenology by Period Across Grouping Levels

Bins vegetation observations into fixed-width calendar periods anchored
at the month-day of `starting_date` (default July 1) and computes the
modal `Phenology` code within each period for several grouping levels.
Periods are defined by the number of days since the most recent anchor
date on or before each observation, so the same seasonal window is
pooled across all years present in the data (e.g., the 'Jul 01-Jul 14'
cell combines that window from every year).

## Usage

``` r
calc_phenology_mode(
  dat,
  starting_date = as.POSIXct(x = "2000-07-01", tz = "UTC"),
  length = 14
)
```

## Arguments

- dat:

  A data frame (or tibble) containing at least the columns `Date` (a
  `POSIXct` column), `Phenology`, `Species`, `Genus`, `Family`,
  `Functional.Group`, and `Growth.Form`.

- starting_date:

  A length-one `POSIXct` giving the seasonal anchor. Only its month and
  day are used; the year is ignored so the anchor generalizes to every
  year in `dat`. Defaults to July 1.

- length:

  Integer. Width of each period in calendar days. Defaults to 14.

## Value

A named list of six data frames. The first five (`Species`, `Genus`,
`Family`, `Functional.Group`, `Growth.Form`) each have one row per level
of that grouping column and one column per period; the sixth (`All`) has
a single row summarizing all observations. Period columns are labelled
by their date range (e.g., `'Jul 01-Jul 14'`) and ordered from the
anchor through the last period containing observations, with no gaps.
Cells contain the modal phenology code, or `NA` where fewer than three
observations exist.

## Details

Within each group-period, the mode is the most frequent phenology code,
computed only when at least three non-missing observations are present
(otherwise the cell is `NA`). Ties are broken by the earliest
phenological stage in the fixed order `N/B < FL < FR < M < C < T < FE`.
`NA` phenology values are ignored when counting and when computing the
mode; they do not trigger an error. Any non-`NA` value outside the seven
allowed codes causes an error before any computation is done.

## Examples

``` r
set.seed(1)
dat <- data.frame(
  Date = as.POSIXct(x = '2022-07-01', tz = 'UTC') +
    sample(
      x = 0:430,
      size = 200,
      replace = T
    ) * 86400,
  Species.Cover.Class = NA_character_,
  Phenology = sample(
    x = c(
      'N/B',
      'FL',
      'FR',
      'M',
      NA
    ),
    size = 200,
    replace = T
  ),
  Species = sample(x = c('Vaccinium membranaceum', 'Chimaphila umbellata'), size = 200, replace = T),
  Genus = sample(x = c('Vaccinium', 'Chimaphila'), size = 200, replace = T),
  Family = sample(x = c('Ericaceae', 'Rosaceae'), size = 200, replace = T),
  Functional.Group = sample(x = c('EVERGREEN SHRUB', 'DECIDUOUS SHRUB'), size = 200, replace = T),
  Growth.Form = 'SHRUB',
  stringsAsFactors = F
)
res <- calc_phenology_mode(dat = dat, length = 14)
res$Genus
#>        Genus Jul 01-Jul 14 Jul 15-Jul 28 Jul 29-Aug 11 Aug 12-Aug 25
#> 1 Chimaphila          <NA>            FL           N/B            FR
#> 2  Vaccinium           N/B          <NA>           N/B           N/B
#>   Aug 26-Sep 08 Sep 09-Sep 22 Sep 23-Oct 06 Oct 07-Oct 20 Oct 21-Nov 03
#> 1           N/B            FR          <NA>            FR          <NA>
#> 2          <NA>          <NA>          <NA>           N/B           N/B
#>   Nov 04-Nov 17 Nov 18-Dec 01 Dec 02-Dec 15 Dec 16-Dec 29 Dec 30-Jan 12
#> 1           N/B          <NA>          <NA>          <NA>          <NA>
#> 2          <NA>            FR             M             M          <NA>
#>   Jan 13-Jan 26 Jan 27-Feb 09 Feb 10-Feb 23 Feb 24-Mar 09 Mar 10-Mar 23
#> 1          <NA>           N/B          <NA>            FR          <NA>
#> 2          <NA>           N/B           N/B          <NA>          <NA>
#>   Mar 24-Apr 06 Apr 07-Apr 20 Apr 21-May 04 May 05-May 18 May 19-Jun 01
#> 1           N/B            FL            FL          <NA>            FL
#> 2          <NA>          <NA>             M          <NA>             M
#>   Jun 02-Jun 15 Jun 16-Jun 29
#> 1           N/B           N/B
#> 2           N/B          <NA>
res$All
#>   All Jul 01-Jul 14 Jul 15-Jul 28 Jul 29-Aug 11 Aug 12-Aug 25 Aug 26-Sep 08
#> 1 All           N/B            FL           N/B           N/B            FL
#>   Sep 09-Sep 22 Sep 23-Oct 06 Oct 07-Oct 20 Oct 21-Nov 03 Nov 04-Nov 17
#> 1            FL          <NA>           N/B           N/B           N/B
#>   Nov 18-Dec 01 Dec 02-Dec 15 Dec 16-Dec 29 Dec 30-Jan 12 Jan 13-Jan 26
#> 1            FR             M             M           N/B          <NA>
#>   Jan 27-Feb 09 Feb 10-Feb 23 Feb 24-Mar 09 Mar 10-Mar 23 Mar 24-Apr 06
#> 1           N/B           N/B            FR          <NA>             M
#>   Apr 07-Apr 20 Apr 21-May 04 May 05-May 18 May 19-Jun 01 Jun 02-Jun 15
#> 1            FL             M          <NA>             M           N/B
#>   Jun 16-Jun 29
#> 1           N/B
```
