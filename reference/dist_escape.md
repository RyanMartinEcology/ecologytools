# Distance to escape terrain

Calculates the distance from each raster cell to the nearest cell
meeting or exceeding a specified slope threshold, interpreted as escape
terrain.

## Usage

``` r
dist_escape(dem, escape_slope, maxdist = 4000)
```

## Arguments

- dem:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  representing elevation.

- escape_slope:

  Numeric scalar giving the slope threshold in degrees.

- maxdist:

  Numeric scalar giving the maximum distance (in map units, typically
  meters) over which to compute distance. Cells farther than `maxdist`
  from the nearest escape-terrain cell are returned as `NA`. Defaults to
  4000 meters. Set to `NA` to compute distances without a cap.

## Value

A
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
giving the distance in meters from each cell to the nearest
escape-terrain cell. Cells beyond `maxdist` are `NA`.

## Details

Slope is calculated from the input DEM using
[`terra::terrain()`](https://rspatial.github.io/terra/reference/terrain.html)
with 8 neighbors and units in degrees. Cells with slope greater than or
equal to `escape_slope` are treated as escape terrain.

## Examples

``` r
if (FALSE) { # \dontrun{
library(terra)

r <- rast(nrows = 100, ncols = 100, xmin = 0, xmax = 100, ymin = 0, ymax = 100)
values(r) <- runif(ncell(r), 0, 1000)

d <- dist_escape(r, escape_slope = 30)
names(d)
plot(d)
} # }
```
