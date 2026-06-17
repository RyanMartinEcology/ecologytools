# Aggregate raster layers into fixed groups

Aggregates consecutive layers of a multilayer
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
into groups of size `n` using a summary function.

## Usage

``` r
aggregate_raster(x, n, fun = mean, cores = 1)
```

## Arguments

- x:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  with multiple layers.

- n:

  Integer scalar giving the number of layers to combine in each group.

- fun:

  Function used to aggregate layers within each group. Defaults to
  `mean`.

- cores:

  Integer number of CPU cores to use. Defaults to `1`.

## Value

A
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
with aggregated layers.

## Details

Layers are grouped in sequential blocks of size `n`. If the number of
layers in `x` is not an exact multiple of `n`, the final group contains
the remaining layers.

Aggregation is performed with
[`terra::tapp()`](https://rspatial.github.io/terra/reference/tapp.html).

## Examples

``` r
if (FALSE) { # \dontrun{
library(terra)

r <- rast(nrows = 10, ncols = 10, nlyrs = 12)
values(r) <- runif(ncell(r) * nlyr(r))

out <- aggregate_raster(r, n = 3, fun = mean)
out
} # }
```
