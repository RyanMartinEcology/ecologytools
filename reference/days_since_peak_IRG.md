# Days since peak IRG

Calculates, for each layer in a multilayer IRG raster, the number of
time steps since the pixel-specific peak IRG value occurred.

## Usage

``` r
days_since_peak_IRG(x, absolute_value = F)
```

## Arguments

- x:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  with multiple layers ordered in time.

- absolute_value:

  Logical. If `TRUE`, returns the absolute value of the output (i.e.,
  distance in time from peak regardless of direction).

## Value

A
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
with the same number of layers as `x`. Values are negative before the
peak, `0` at the peak, and positive after the peak unless
`absolute_value = TRUE`.

## Details

The peak layer is identified separately for each cell using
[`terra::which.max()`](https://rspatial.github.io/terra/reference/summarize-generics.html).
The output is calculated as:

`layer_index - peak_index`

If `absolute_value = TRUE`, the result is transformed to:

`abs(layer_index - peak_index)`

## Examples

``` r
if (FALSE) { # \dontrun{
library(terra)

r <- rast(nrows = 10, ncols = 10, nlyrs = 5)
values(r) <- runif(ncell(r) * nlyr(r))

out <- days_since_peak_IRG(r)
out_abs <- days_since_peak_IRG(r, absolute_value = TRUE)
} # }
```
