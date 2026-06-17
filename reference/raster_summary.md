# Print a formatted summary of a SpatRaster object

Prints a neatly formatted diagnostic summary of a `SpatRaster` or
`PackedSpatRaster` object to the console. Useful for quickly inspecting
raster properties during data preparation, debugging, or pipeline
validation. If a `PackedSpatRaster` is provided, it is automatically
unwrapped before inspection.

## Usage

``` r
raster_summary(r, name = NULL)
```

## Arguments

- r:

  A `SpatRaster` or `PackedSpatRaster` object.

- name:

  An optional character string giving a label for the raster, printed as
  the summary header. Defaults to `NULL`, in which case no label is
  printed.

## Value

Invisibly returns a named list of the summary values. Called primarily
for its side effect of printing to the console.

## Details

The following properties are reported:

- **Class**: whether the input was a `SpatRaster` or `PackedSpatRaster`

- **Layers**: number of layers

- **Cells**: total number of cells

- **Resolution**: x and y resolution

- **Extent**: xmin, xmax, ymin, ymax

- **CRS**: proj4 string of the coordinate reference system

- **Source**: file path of the raster source, or empty if in memory

- **In memory**: whether the raster data is held in memory

- **Has time**: whether the raster has time attributes

- **Value range**: minimum and maximum cell values

## See also

[`describe`](https://rspatial.github.io/terra/reference/describe.html),
[`force_inmemory`](https://ryanmartinecology.github.io/ecologytools/reference/force_inmemory.md),
`safe_unwrap`

## Examples

``` r
if (FALSE) { # \dontrun{
library(terra)

r <- rast(nrows = 10, ncols = 10, vals = runif(100))
raster_summary(r, name = "my_raster")

# Works on PackedSpatRaster too
rp <- wrap(r)
raster_summary(rp, name = "packed_raster")
} # }
```
