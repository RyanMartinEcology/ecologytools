# Force a SpatRaster into memory

Reads all raster values from their source (file or memory) and stores
them directly in the SpatRaster object. This is necessary before
serializing a SpatRaster with
[`wrap()`](https://rspatial.github.io/terra/reference/wrap.html) and
[`saveRDS()`](https://rspatial.github.io/terra/reference/serialize.html),
as rasters that reference local file paths will produce null pointer
errors when loaded in a new R session or on a different machine (e.g. a
compute cluster).

## Usage

``` r
force_inmemory(r)
```

## Arguments

- r:

  A `SpatRaster` object.

## Value

A `SpatRaster` object with all values stored in memory.

## Details

`SpatRaster` objects in `terra` may reference external file paths rather
than holding data in memory. When such objects are serialized via
[`wrap()`](https://rspatial.github.io/terra/reference/wrap.html) and
[`saveRDS()`](https://rspatial.github.io/terra/reference/serialize.html),
the file path reference is preserved but becomes invalid in any session
where that path does not exist. Calling `force_inmemory()` before
wrapping ensures data is embedded in the serialized object rather than
referenced by path.

The function checks
[`inMemory()`](https://rspatial.github.io/terra/reference/sources.html)
before reading values, so it is safe to call on rasters that are already
in memory without redundant data reads.

## See also

[`inMemory`](https://rspatial.github.io/terra/reference/sources.html),
[`wrap`](https://rspatial.github.io/terra/reference/wrap.html)

## Examples

``` r
if (FALSE) { # \dontrun{
library(terra)

# Single raster
r <- rast("path/to/raster.tif")
r_wrapped <- wrap(force_inmemory(r))
saveRDS(r_wrapped, "raster.rds")

# List of rasters
raster_list <- lapply(raster_list, function(r) wrap(force_inmemory(r)))
saveRDS(raster_list, "raster_list.rds")
} # }
```
