#' Distance to escape terrain
#'
#' Calculates the distance from each raster cell to the nearest cell meeting
#' or exceeding a specified slope threshold, interpreted as escape terrain.
#'
#' @param dem A `terra::SpatRaster` representing elevation.
#' @param escape_slope Numeric scalar giving the slope threshold in degrees.
#' @param maxdist Numeric scalar giving the maximum distance (in map units,
#' typically meters) over which to compute distance. Cells farther than
#' `maxdist` from the nearest escape-terrain cell are returned as `NA`.
#' Defaults to 4000 meters. Set to `NA` to compute distances without a cap.
#'
#' @return A `terra::SpatRaster` giving the distance in meters from each cell
#' to the nearest escape-terrain cell. Cells beyond `maxdist` are `NA`.
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

dist_escape <- function(dem, escape_slope, maxdist = 4000) {

  # ----------------------------------------------------------------------------------------------------------------------
  # validate inputs
  # ----------------------------------------------------------------------------------------------------------------------

  #1) require a SpatRaster DEM
  stopifnot(inherits(x = dem, what = 'SpatRaster'))

  #2) require a single non-missing slope threshold
  stopifnot(
    is.numeric(escape_slope),
    length(escape_slope) == 1,
    !is.na(escape_slope)
  )

  # ----------------------------------------------------------------------------------------------------------------------
  # identify escape terrain
  # ----------------------------------------------------------------------------------------------------------------------

  #1) derive slope in degrees from the DEM
  slope <- terra::terrain(
    x = dem,
    v = 'slope',
    unit = 'degrees',
    neighbors = 8
  )

  #2) flag cells meeting or exceeding the threshold
  escape <- slope >= escape_slope
  escape_for_dist <- terra::ifel(test = escape, yes = 1, no = NA)

  #3) require at least one escape-terrain cell
  n_escape <- terra::global(x = !is.na(escape_for_dist), fun = 'sum', na.rm = T)[1, 1]
  if (is.na(n_escape) || n_escape == 0) {
    stop('No cells meet or exceed escape_slope; cannot compute distance to escape terrain.')
  }

  # ----------------------------------------------------------------------------------------------------------------------
  # compute distance to escape terrain
  # ----------------------------------------------------------------------------------------------------------------------

  #1) measure distance to the nearest escape-terrain cell
  dist_m <- terra::distance(x = escape_for_dist, maxdist = maxdist)

  #2) label and return the result
  names(dist_m) <- 'dist_escape_m'
  dist_m
}
