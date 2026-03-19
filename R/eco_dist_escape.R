#' Distance to escape terrain
#'
#' Calculates the distance from each raster cell to the nearest cell meeting
#' or exceeding a specified slope threshold, interpreted as escape terrain.
#'
#' @param dem A `terra::SpatRaster` representing elevation.
#' @param escape_slope Numeric scalar giving the slope threshold in degrees.
#'
#' @return A `terra::SpatRaster` giving the distance in meters from each cell
#' to the nearest escape-terrain cell.
#'
#' @details
#' Slope is calculated from the input DEM using `terra::terrain()` with
#' 8 neighbors and units in degrees. Cells with slope greater than or equal
#' to `escape_slope` are treated as escape terrain.
#'
#' @examples
#' \dontrun{
#' library(terra)
#'
#' r <- rast(nrows = 100, ncols = 100, xmin = 0, xmax = 100, ymin = 0, ymax = 100)
#' values(r) <- runif(ncell(r), 0, 1000)
#'
#' d <- dist_escape(r, escape_slope = 30)
#' names(d)
#' plot(d)
#' }
#'
#' @export

dist_escape <- function(dem, escape_slope) {
  stopifnot(inherits(dem, "SpatRaster"))
  stopifnot(is.numeric(escape_slope), length(escape_slope) == 1, !is.na(escape_slope))

  slope <- terra::terrain(dem, v = "slope", unit = "degrees", neighbors = 8)
  escape <- slope >= escape_slope
  escape_for_dist <- terra::ifel(escape, 1, NA)

  n_escape <- terra::global(!is.na(escape_for_dist), "sum", na.rm = TRUE)[1, 1]

  if (is.na(n_escape) || n_escape == 0) {
    stop("No cells meet or exceed escape_slope; cannot compute distance to escape terrain.")
  }

  dist_m <- terra::distance(escape_for_dist)
  names(dist_m) <- "dist_escape_m"
  dist_m
}
