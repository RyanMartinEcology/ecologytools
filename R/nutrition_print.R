
#' Format method for fresh_output objects
#'
#' Controls the description displayed in the RStudio Environment pane.
#'
#' @param x A \code{fresh_output} object.
#' @param ... Not used.
#'
#' @export

format.fresh_output <- function(x, ...) {
  n_patches <- nrow(x$summary)
  n_forages <- nrow(x$detail)
  area_unit <- x$inputs$output_area_unit
  paste0(
    'FRESH output [',
    n_patches, ' patch(es), ',
    n_forages, ' forage(s), ',
    'unit: ', area_unit,
    ']'
  )
}


#' Print method for fresh_output objects
#'
#' Controls console output when a \code{fresh_output} object is typed or
#' explicitly printed.
#'
#' @param x A \code{fresh_output} object.
#' @param ... Not used.
#'
#' @export

print.fresh_output <- function(x, ...) {
  cat('FRESH output\n')
  cat('  Patches  :', nrow(x$summary), '\n')
  cat('  Forages  :', nrow(x$detail), '\n')
  cat('  Area unit:', x$inputs$output_area_unit, '\n')
  cat('\n$summary:\n')
  print(x$summary)
  invisible(x)
}
