#' Force a SpatRaster into memory
#'
#' @description
#' Reads all raster values from their source (file or memory) and stores them
#' directly in the SpatRaster object. This is necessary before serializing
#' a SpatRaster with \code{wrap()} and \code{saveRDS()}, as rasters that
#' reference local file paths will produce null pointer errors when loaded
#' in a new R session or on a different machine (e.g. a compute cluster).
#'
#' @param r A \code{SpatRaster} object.
#'
#' @return A \code{SpatRaster} object with all values stored in memory.
#'
#' @details
#' \code{SpatRaster} objects in \code{terra} may reference external file paths
#' rather than holding data in memory. When such objects are serialized via
#' \code{wrap()} and \code{saveRDS()}, the file path reference is preserved but
#' becomes invalid in any session where that path does not exist. Calling
#' \code{force_inmemory()} before wrapping ensures data is embedded in the
#' serialized object rather than referenced by path.
#'
#' The function checks \code{inMemory()} before reading values, so it is safe
#' to call on rasters that are already in memory without redundant data reads.
#'
#' @examples
#' \dontrun{
#' library(terra)
#'
#' # Single raster
#' r <- rast("path/to/raster.tif")
#' r_wrapped <- wrap(force_inmemory(r))
#' saveRDS(r_wrapped, "raster.rds")
#'
#' # List of rasters
#' raster_list <- lapply(raster_list, function(r) wrap(force_inmemory(r)))
#' saveRDS(raster_list, "raster_list.rds")
#' }
#'
#' @seealso \code{\link[terra]{inMemory}}, \code{\link[terra]{wrap}}
#'
#' @importFrom terra inMemory values
#' @export


force_inmemory <- function(r) {
  if (!inMemory(r)) values(r) <- values(r)
  r
}
