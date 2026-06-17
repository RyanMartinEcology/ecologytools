#' Aggregate raster layers into fixed groups
#'
#' Aggregates consecutive layers of a multilayer `terra::SpatRaster` into
#' groups of size `n` using a summary function.
#'
#' @param x A `terra::SpatRaster` with multiple layers.
#' @param n Integer scalar giving the number of layers to combine in each group.
#' @param fun Function used to aggregate layers within each group. Defaults to
#' `mean`.
#' @param cores Integer number of CPU cores to use. Defaults to `1`.
#'
#' @return A `terra::SpatRaster` with aggregated layers.
#'
#' @details
#' Layers are grouped in sequential blocks of size `n`. If the number of layers
#' in `x` is not an exact multiple of `n`, the final group contains the remaining
#' layers.
#'
#' Aggregation is performed with `terra::tapp()`.
#'
#' @examples
#' \dontrun{
#' library(terra)
#'
#' r <- rast(nrows = 10, ncols = 10, nlyrs = 12)
#' values(r) <- runif(ncell(r) * nlyr(r))
#'
#' out <- aggregate_raster(r, n = 3, fun = mean)
#' out
#' }
#'
#' @export

aggregate_raster <- function(x, n, fun = mean, cores = 1) {

  # ----------------------------------------------------------------------------------------------------------------------
  # validate inputs
  # ----------------------------------------------------------------------------------------------------------------------

  #1) require a SpatRaster
  stopifnot(inherits(x = x, what = 'SpatRaster'))

  #2) require a single positive whole-number group size
  stopifnot(
    is.numeric(n),
    length(n) == 1,
    !is.na(n),
    n > 0,
    n %% 1 == 0
  )

  #3) require an aggregation function
  stopifnot(is.function(fun))

  #4) require a single positive whole-number core count
  stopifnot(
    is.numeric(cores),
    length(cores) == 1,
    !is.na(cores),
    cores > 0,
    cores %% 1 == 0
  )

  #5) require at least one layer
  n_layers <- terra::nlyr(x)
  if (n_layers < 1) {
    stop('x must have at least 1 layer.')
  }

  # ----------------------------------------------------------------------------------------------------------------------
  # build the grouping index
  # ----------------------------------------------------------------------------------------------------------------------

  #1) assign full blocks of size n
  full_periods <- n_layers %/% n
  grouping <- rep(x = seq_len(full_periods), each = n)

  #2) append a trailing partial block when layers do not divide evenly
  remaining_layers <- n_layers %% n
  if (remaining_layers > 0) {
    grouping <- c(grouping, rep(x = full_periods + 1, times = remaining_layers))
  }

  # ----------------------------------------------------------------------------------------------------------------------
  # aggregate layers
  # ----------------------------------------------------------------------------------------------------------------------

  #1) summarize each group with the supplied function
  terra::tapp(
    x = x,
    index = grouping,
    fun = fun,
    cores = cores
  )
}
