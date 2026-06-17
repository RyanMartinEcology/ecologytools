#' Vector ruggedness measure (VRM)
#'
#' Computes the vector ruggedness measure (VRM) of Sappington et al. (2007),
#' quantifying terrain ruggedness as the three-dimensional dispersion of unit
#' vectors orthogonal to the terrain surface within a focal window.
#'
#' @param x A `terra::SpatRaster` representing a digital elevation model (DEM).
#' @param s Odd integer (or length-2 vector of odd integers) giving the focal
#'   window size in rows and columns.
#'
#' @return A `terra::SpatRaster` of vector ruggedness values, bounded between
#'   0 (flat) and 1 (maximally rugged).
#'
#' @details
#' VRM integrates variation in both slope and aspect into a single
#' dimensionless measure of surface heterogeneity that is less correlated
#' with slope than traditional ruggedness indices. This allows ruggedness
#' and slope to be treated as separate ecological predictors.
#'
#' This implementation is a direct port of `spatialEco::vrm()`, reproduced
#' here so that `ecologytools` does not need to depend on `spatialEco`. The
#' algorithm, defaults, and output are identical.
#'
#' @references
#' Sappington, J. M., K. M. Longshore, and D. B. Thompson. 2007.
#' Quantifying landscape ruggedness for animal habitat analysis:
#' a case study using bighorn sheep in the Mojave Desert.
#' *Journal of Wildlife Management* 71:1419–1426.
#' https://doi.org/10.2193/2005-723
#'
#' @examples
#' \dontrun{
#' library(terra)
#'
#' r <- rast(nrows = 100, ncols = 100, xmin = 0, xmax = 100, ymin = 0, ymax = 100)
#' xy <- as.data.frame(crds(r))
#' z <- with(
#'   xy,
#'   800 +
#'     250 * exp(-((x - 30)^2 + (y - 35)^2) / 250) +
#'     180 * exp(-((x - 70)^2 + (y - 65)^2) / 180)
#' )
#' values(r) <- z
#'
#' out <- vrm(r, s = 5)
#' plot(out)
#' }
#'
#' @export
vrm <- function (x, s)
{

  # ----------------------------------------------------------------------------------------------------------------------
  # validate inputs
  # ----------------------------------------------------------------------------------------------------------------------

  #1) require a terra SpatRaster
  if (!inherits(x = x, what = 'SpatRaster'))
    stop(deparse(substitute(x)), ' must be a terra SpatRaster object')

  #2) require at most two window dimensions
  if (length(s) > 2)
    stop('Specified window exceeds 2 dimensions')

  #3) require odd window size(s)
  if (any((s%%2) == 0))
    stop('Specified window must be odd number(s)')

  #4) recycle a scalar window to both dimensions
  if (length(s) == 1)
    s = rep(x = s, times = 2)

  # ----------------------------------------------------------------------------------------------------------------------
  # build focal window and vector helper
  # ----------------------------------------------------------------------------------------------------------------------

  #1) define the resultant-vector magnitude helper
  vrm.fun <- function(x, y, z) {
    sqrt((x^2) + (y^2) + (z^2))
  }

  #2) build the focal weight matrix and scale factor
  f = matrix(data = 1, nrow = s[1], ncol = s[2])
  scale.factor <- round(x = s[1] * s[2], digits = 0)

  # ----------------------------------------------------------------------------------------------------------------------
  # decompose slope and aspect into unit vectors
  # ----------------------------------------------------------------------------------------------------------------------

  #1) compute slope and aspect in radians
  sa <- terra::terrain(
    x = x,
    v = c('slope', 'aspect'),
    unit = 'radians',
    neighbors = 8
  )

  #2) take sine and cosine of slope
  sin.slp <- terra::app(x = sa[['slope']], fun = sin)
  cos.slp <- terra::app(x = sa[['slope']], fun = cos)

  #3) project aspect onto the horizontal plane
  sin.asp <- terra::app(x = sa[['aspect']], fun = sin) * sin.slp
  cos.asp <- terra::app(x = sa[['aspect']], fun = cos) * sin.slp

  # ----------------------------------------------------------------------------------------------------------------------
  # sum vectors and return ruggedness
  # ----------------------------------------------------------------------------------------------------------------------

  #1) sum each vector component within the focal window
  x.sum <- terra::focal(x = sin.asp, w = f, fun = sum)
  y.sum <- terra::focal(x = cos.asp, w = f, fun = sum)
  z.sum <- terra::focal(x = cos.slp, w = f, fun = sum)

  #2) compute the resultant vector magnitude
  r <- terra::lapp(x = c(x.sum, y.sum, z.sum), fun = vrm.fun)

  #3) return ruggedness as one minus the normalized magnitude
  return(1 - (r/scale.factor))
}


#' Vector ruggedness of local relief (VRML)
#'
#' Computes a local-relief version of the vector ruggedness measure (VRM)
#' by first removing broad-scale topography and then quantifying fine-scale
#' terrain heterogeneity.
#'
#' @param x A `terra::SpatRaster` representing a digital elevation model (DEM).
#' @param s Odd integer giving the focal window size.
#'
#' @return A `terra::SpatRaster` of vector ruggedness values representing
#' local-scale terrain heterogeneity.
#'
#' @details
#' The standard vector ruggedness measure (VRM; Sappington et al. 2007)
#' quantifies terrain ruggedness as the three-dimensional dispersion of
#' vectors orthogonal to the terrain surface, integrating variation in both
#' slope and aspect. This produces a dimensionless measure of surface
#' heterogeneity that is less correlated with slope than traditional
#' ruggedness indices.
#'
#' The `vrml()` function modifies this approach by first smoothing the DEM
#' using a focal mean filter and subtracting the original DEM. This isolates
#' fine-scale (local) topographic variation by removing broad-scale elevation
#' trends. Vector ruggedness is then computed on this residual surface using
#' [vrm()].
#'
#' As a result:
#' - [vrm()] measures total terrain heterogeneity across scales
#'   present in the DEM.
#' - `vrml()` emphasizes *local relief* by removing low-frequency structure
#'   (e.g., large slopes or elevational gradients) prior to calculating
#'   ruggedness.
#'
#' This distinction is important because VRM captures variation in slope and
#' aspect independent of overall gradient, allowing ruggedness and slope to
#' be treated as separate ecological predictors. By
#' applying VRM to a detrended surface, `vrml()` further isolates microtopographic
#' complexity that may be more relevant to processes operating at finer spatial
#' scales (e.g., movement constraints, microhabitat selection).
#'
#' @references
#' Sappington, J. M., K. M. Longshore, and D. B. Thompson. 2007.
#' Quantifying landscape ruggedness for animal habitat analysis:
#' a case study using bighorn sheep in the Mojave Desert.
#' *Journal of Wildlife Management* 71:1419–1426.
#' https://doi.org/10.2193/2005-723
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

  # ----------------------------------------------------------------------------------------------------------------------
  # validate inputs
  # ----------------------------------------------------------------------------------------------------------------------

  #1) require a terra SpatRaster
  stopifnot(inherits(x = x, what = 'SpatRaster'))

  #2) require a single positive window size
  stopifnot(is.numeric(s), length(s) == 1, !is.na(s), s > 0)

  #3) require an odd window size
  if (s %% 2 != 1) {
    stop('s must be an odd integer.')
  }

  # ----------------------------------------------------------------------------------------------------------------------
  # detrend and compute local ruggedness
  # ----------------------------------------------------------------------------------------------------------------------

  #1) smooth the dem with a focal mean filter
  smooth <- terra::focal(
    x = x,
    w = s,
    fun = 'mean',
    na.policy = 'omit'
  )

  #2) remove broad-scale topography to isolate local relief
  diff <- smooth - x

  #3) compute vector ruggedness on the residual surface
  out <- vrm(x = diff, s = s)
  out
}
