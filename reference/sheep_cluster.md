# Cluster recent sheep GPS locations into search areas

Creates recent last locations, movement paths, and DBSCAN-based convex
hull search areas from GPS data. Optionally writes outputs as KML or GPX
and produces a basic plot.

## Usage

``` r
sheep_cluster(
  GPS,
  no.days = 7,
  reference_date = Sys.time(),
  minimum.points = 10,
  epsilon = 100,
  crs = "EPSG:32612",
  filetype = "KML",
  last.point = T,
  trace = T,
  basic.plot = F
)
```

## Arguments

- GPS:

  A data frame containing columns `x`, `y`, `ID`, and `DateTime`. `x`
  and `y` should be projected coordinates in the supplied `crs`.
  `DateTime` must be a `POSIXct` vector.

- no.days:

  Numeric. Number of days before `reference_date` to retain.

- reference_date:

  A `POSIXct` reference date. Defaults to current time.

- minimum.points:

  Integer. Minimum number of points required by DBSCAN to define a
  cluster.

- epsilon:

  Numeric. DBSCAN epsilon parameter, in map units of `crs`.

- crs:

  Character string giving the coordinate reference system, passed to
  [`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html).
  Defaults to `"EPSG:32612"`.

- filetype:

  Character. Output vector file type. Must be either `"KML"` or `"GPX"`.

- last.point:

  Logical. If `TRUE`, write last locations for each individual.

- trace:

  Logical. If `TRUE`, write movement paths for each individual.

- basic.plot:

  Logical. If `TRUE`, draw a simple plot of search areas, movement
  paths, and last locations.

## Value

A
[`terra::SpatVector`](https://rspatial.github.io/terra/reference/SpatVector-class.html)
of search areas if clusters are found. If no clusters are found, returns
the point `SpatVector` with attached `clusterID`.

## Details

The function filters GPS locations to observations occurring within
`no.days` before `reference_date`, then applies DBSCAN clustering to all
retained points. Minimum convex hulls are drawn around non-noise
clusters.

Output files are written to the working directory as:

- `last_locations.<ext>`

- `movement_paths.<ext>`

- `search_areas.<ext>`

where `<ext>` is determined by `filetype`.

GPX support for polygon outputs depends on the software used to read the
file, and is generally less robust than KML for search-area polygons.

## Examples

``` r
if (FALSE) { # \dontrun{
out <- sheep_cluster(
  GPS = gps_df,
  no.days = 7,
  minimum.points = 10,
  epsilon = 100,
  crs = "EPSG:32612",
  filetype = "KML"
)
} # }
```
