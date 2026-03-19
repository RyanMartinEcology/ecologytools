#' Days since peak IRG
#'
#' Calculates, for each layer in a multilayer IRG raster, the number of
#' time steps since the pixel-specific peak IRG value occurred.
#'
#' @param x A `terra::SpatRaster` with multiple layers ordered in time.
#' @param absolute_value Logical. If `TRUE`, returns the absolute value of
#' the output (i.e., distance in time from peak regardless of direction).
#'
#' @return A `terra::SpatRaster` with the same number of layers as `x`.
#' Values are negative before the peak, `0` at the peak, and positive after
#' the peak unless `absolute_value = TRUE`.
#'
#' @details
#' The peak layer is identified separately for each cell using
#' `terra::which.max()`. The output is calculated as:
#'
#' `layer_index - peak_index`
#'
#' If `absolute_value = TRUE`, the result is transformed to:
#'
#' `abs(layer_index - peak_index)`
#'
#' @examples
#' \dontrun{
#' library(terra)
#'
#' r <- rast(nrows = 10, ncols = 10, nlyrs = 5)
#' values(r) <- runif(ncell(r) * nlyr(r))
#'
#' out <- days_since_peak_IRG(r)
#' out_abs <- days_since_peak_IRG(r, absolute_value = TRUE)
#' }
#'
#' @export

days_since_peak_IRG <- function(x, absolute_value = FALSE) {
  stopifnot(inherits(x, "SpatRaster"))
  stopifnot(is.logical(absolute_value), length(absolute_value) == 1, !is.na(absolute_value))

  if (terra::nlyr(x) < 2) {
    stop("x must have at least 2 layers.")
  }

  # peak layer index for each cell
  peak_index <- terra::which.max(x)

  # layer index raster stack
  layer_index <- terra::rast(x)
  terra::values(layer_index) <- rep(seq_len(terra::nlyr(x)), each = terra::ncell(x))

  # compute difference
  out <- layer_index - peak_index

  # optional absolute value
  if (absolute_value) {
    out <- abs(out)
  }

  names(out) <- paste0("days_since_peak_IRG_", seq_len(terra::nlyr(x)))

  out
}
