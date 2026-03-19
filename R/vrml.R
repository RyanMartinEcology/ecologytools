#' Vector ruggedness of local relief
#'
#' Calculates a smoothed DEM, subtracts the original DEM to isolate local
#' topographic variation, and then computes vector ruggedness measure on the
#' residual surface.
#'
#' @param x A `terra::SpatRaster` DEM.
#' @param s Odd integer giving the focal window size.
#'
#' @return A `terra::SpatRaster` of vector ruggedness values.
#'
#' @details
#' The DEM is first smoothed with `terra::focal()` using a mean filter of size
#' `s`. The smoothed surface is then subtracted from the original DEM to isolate
#' local topographic variation. Vector ruggedness measure is then calculated on
#' that residual raster using `spatialEco::vrm()`.
#'
#' @examples
#' \dontrun{
#' library(terra)
#'
#' r <- rast(nrows = 100, ncols = 100, xmin = 0, xmax = 100, ymin = 0, ymax = 100)
#'
#' xy <- as.data.frame(crds(r))
#' z <- with(
#'   xy,
#'   800 +
#'     250 * exp(-((x - 30)^2 + (y - 35)^2) / 250) +
#'     180 * exp(-((x - 70)^2 + (y - 65)^2) / 180) -
#'     120 * exp(-((x - 55)^2 + (y - 45)^2) / 120) +
#'     60 * sin(x / 8) * cos(y / 10)
#' )
#'
#' values(r) <- z
#'
#' out <- vrml(r, s = 5)
#' plot(out)
#' }
#'
#' @export


vrml <- function(x, s) {
  stopifnot(inherits(x, "SpatRaster"))
  stopifnot(is.numeric(s), length(s) == 1, !is.na(s), s > 0)

  if (s %% 2 != 1) {
    stop("s must be an odd integer.")
  }

  smooth <- terra::focal(
    x = x,
    w = s,
    fun = "mean",
    na.policy = "omit"
  )

  diff <- smooth - x

  out <- spatialEco::vrm(diff, s = s)

  out
}
