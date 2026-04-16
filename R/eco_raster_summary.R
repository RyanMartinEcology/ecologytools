#' Print a formatted summary of a SpatRaster object
#'
#' @description
#' Prints a neatly formatted diagnostic summary of a \code{SpatRaster} or
#' \code{PackedSpatRaster} object to the console. Useful for quickly inspecting
#' raster properties during data preparation, debugging, or pipeline validation.
#' If a \code{PackedSpatRaster} is provided, it is automatically unwrapped before
#' inspection.
#'
#' @param r A \code{SpatRaster} or \code{PackedSpatRaster} object.
#' @param name An optional character string giving a label for the raster,
#'   printed as the summary header. Defaults to \code{NULL}, in which case
#'   no label is printed.
#'
#' @return Invisibly returns a named list of the summary values. Called
#'   primarily for its side effect of printing to the console.
#'
#' @details
#' The following properties are reported:
#' \itemize{
#'   \item \strong{Class}: whether the input was a \code{SpatRaster} or \code{PackedSpatRaster}
#'   \item \strong{Layers}: number of layers
#'   \item \strong{Cells}: total number of cells
#'   \item \strong{Resolution}: x and y resolution
#'   \item \strong{Extent}: xmin, xmax, ymin, ymax
#'   \item \strong{CRS}: proj4 string of the coordinate reference system
#'   \item \strong{Source}: file path of the raster source, or empty if in memory
#'   \item \strong{In memory}: whether the raster data is held in memory
#'   \item \strong{Has time}: whether the raster has time attributes
#'   \item \strong{Value range}: minimum and maximum cell values
#' }
#'
#' @examples
#' \dontrun{
#' library(terra)
#'
#' r <- rast(nrows = 10, ncols = 10, vals = runif(100))
#' raster_summary(r, name = "my_raster")
#'
#' # Works on PackedSpatRaster too
#' rp <- wrap(r)
#' raster_summary(rp, name = "packed_raster")
#' }
#'
#' @seealso \code{\link[terra]{describe}}, \code{\link{force_inmemory}},
#'   \code{\link{safe_unwrap}}
#'
#' @importFrom terra nlyr ncell res ext crs sources inMemory has.time values
#' @export

raster_summary <- function(r, name = NULL) {

  input_class <- class(r)[1]
  r <- safe_unwrap(r)

  ext_vals <- as.vector(ext(r))
  val_range <- tryCatch(
    range(values(r), na.rm = TRUE),
    error = function(e) c(NA, NA)
  )

  out <- list(
    class    = input_class,
    nlyr     = nlyr(r),
    ncell    = ncell(r),
    res      = res(r),
    ext      = ext_vals,
    crs      = crs(r, proj = TRUE),
    source   = sources(r),
    memory   = inMemory(r),
    has_time = has.time(r),
    range    = val_range
  )

  cat("============================================================\n")
  if (!is.null(name)) {
    cat(sprintf("  Raster Summary: %s\n", name))
    cat("------------------------------------------------------------\n")
  } else {
    cat("  Raster Summary\n")
    cat("------------------------------------------------------------\n")
  }
  cat(sprintf("  %-15s %s\n",  "Class:",      out$class))
  cat(sprintf("  %-15s %d\n",  "Layers:",     out$nlyr))
  cat(sprintf("  %-15s %s\n",  "Cells:",      formatC(out$ncell, format = "d", big.mark = ",")))
  cat(sprintf("  %-15s %.4f x %.4f\n", "Resolution:", out$res[1], out$res[2]))
  cat(sprintf("  %-15s xmin: %-12.4f xmax: %.4f\n", "Extent:", ext_vals[1], ext_vals[2]))
  cat(sprintf("  %-15s ymin: %-12.4f ymax: %.4f\n", "",         ext_vals[3], ext_vals[4]))
  cat(sprintf("  %-15s %s\n",  "CRS:",        ifelse(nchar(out$crs) > 0, out$crs, "NA")))
  cat(sprintf("  %-15s %s\n",  "Source:",     ifelse(nchar(out$source) > 0, out$source, "in memory")))
  cat(sprintf("  %-15s %s\n",  "In memory:",  ifelse(out$memory, "yes", "no")))
  cat(sprintf("  %-15s %s\n",  "Has time:",   ifelse(out$has_time, "yes", "no")))
  cat(sprintf("  %-15s %.4f to %.4f\n", "Value range:", out$range[1], out$range[2]))
  cat("============================================================\n")

  invisible(out)
}
