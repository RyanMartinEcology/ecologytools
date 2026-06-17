#' Time the execution of an expression
#'
#' @description
#' Wraps any R expression and reports how long it took to execute, printing
#' a formatted message to the console with the start time, end time, and
#' elapsed duration. Useful for profiling long-running operations in scripts
#' or cluster jobs.
#'
#' @param expr An R expression to evaluate.
#' @param label An optional character string describing the expression, used
#'   in the console output. Defaults to \code{NULL}, in which case the
#'   deparsed expression is used as the label.
#'
#' @return Invisibly returns the result of evaluating \code{expr}. Called
#'   primarily for its side effect of printing timing information to the
#'   console.
#'
#' @examples
#' \dontrun{
#' # Time a simple operation
#' result <- time_it(Sys.sleep(2), label = "sleeping")
#'
#' # Time a model run
#' micropointa <- time_it(
#'   runpointmodela(...),
#'   label = "micropoint model"
#' )
#' }
#'
#' @export

time_it <- function(expr, label = NULL) {

  # ----------------------------------------------------------------------------------------------------------------------
  # resolve the label
  # ----------------------------------------------------------------------------------------------------------------------

  #1) use the supplied label, else deparse the expression
  label <- if (!is.null(label)) label else base::deparse(base::substitute(expr))

  # ----------------------------------------------------------------------------------------------------------------------
  # time the expression
  # ----------------------------------------------------------------------------------------------------------------------

  #1) announce the start
  base::cat(base::sprintf('  [%s] Starting: %s\n', base::format(x = base::Sys.time(), format = '%H:%M:%S'), label))

  #2) evaluate the expression and measure elapsed time
  t0 <- base::proc.time()
  result <- base::force(expr)
  elapsed <- (base::proc.time() - t0)[['elapsed']]

  #3) announce completion with the elapsed duration
  base::cat(base::sprintf('  [%s] Done: %s (%.2f seconds)\n',
                          base::format(x = base::Sys.time(), format = '%H:%M:%S'), label, elapsed))

  #4) return the result invisibly
  base::invisible(result)
}


#' Print information about loaded packages
#'
#' @description
#' Prints a formatted summary of the current R session, including the R
#' version, platform, and the names and versions of all currently loaded
#' packages. Useful for reproducibility logging at the top of cluster scripts
#' or in analysis pipelines where tracking package versions is important.
#'
#' @return Invisibly returns a named list of loaded package information, where
#'   each element is the package description object returned by
#'   \code{\link[utils]{sessionInfo}}. Called primarily for its side effect
#'   of printing to the console.
#'
#' @details
#' Only packages attached via \code{library()} or \code{require()} are
#' reported — base packages and packages loaded but not attached are excluded.
#' For a full session snapshot including base packages, see
#' \code{\link[utils]{sessionInfo}}.
#'
#' @examples
#' \dontrun{
#' library(terra)
#' library(microclimf)
#'
#' package_info()
#' }
#'
#' @seealso \code{\link[utils]{sessionInfo}}, \code{\link{print_script_header}}
#'
#' @importFrom utils sessionInfo
#' @export

package_info <- function() {

  # ----------------------------------------------------------------------------------------------------------------------
  # collect session details
  # ----------------------------------------------------------------------------------------------------------------------

  #1) grab the attached (non-base) packages
  pkgs <- utils::sessionInfo()$otherPkgs

  # ----------------------------------------------------------------------------------------------------------------------
  # print the session header
  # ----------------------------------------------------------------------------------------------------------------------

  #1) print the version, platform, and run time
  base::cat('============================================================\n')
  base::cat('  Package Info\n')
  base::cat(base::sprintf('  R version: %s\n', base::R.version$version.string))
  base::cat(base::sprintf('  Platform:  %s\n', base::R.version$platform))
  base::cat(base::sprintf('  Running:   %s\n', base::format(x = base::Sys.time(), format = '%Y-%m-%d %H:%M:%S')))

  # ----------------------------------------------------------------------------------------------------------------------
  # print the loaded packages
  # ----------------------------------------------------------------------------------------------------------------------

  #1) list each package and its version
  base::cat('------------------------------------------------------------\n')
  base::cat('  Loaded packages:\n')
  for (pkg in base::names(pkgs)) {
    base::cat(base::sprintf('    %-25s %s\n', pkg, pkgs[[pkg]]$Version))
  }
  base::cat('============================================================\n')

  #2) return the package list invisibly
  base::invisible(pkgs)
}
