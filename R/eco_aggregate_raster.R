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
  stopifnot(inherits(x, "SpatRaster"))
  stopifnot(
    is.numeric(n),
    length(n) == 1,
    !is.na(n),
    n > 0,
    n %% 1 == 0
  )
  stopifnot(is.function(fun))
  stopifnot(
    is.numeric(cores),
    length(cores) == 1,
    !is.na(cores),
    cores > 0,
    cores %% 1 == 0
  )

  n_layers <- terra::nlyr(x)

  if (n_layers < 1) {
    stop("x must have at least 1 layer.")
  }

  full_periods <- n_layers %/% n
  grouping <- rep(seq_len(full_periods), each = n)

  remaining_layers <- n_layers %% n
  if (remaining_layers > 0) {
    grouping <- c(grouping, rep(full_periods + 1, remaining_layers))
  }

  terra::tapp(x, grouping, fun = fun, cores = cores)
}
