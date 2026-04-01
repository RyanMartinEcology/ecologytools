.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "ecologytools ", utils::packageVersion("ecologytools"), "\n",
    "Utilities and Data Visualization Tools for Ecology\n",
    "Functions: theme_martin(),\n vrml(),\n dist_escape(),\n days_since_peak_IRG(),\n aggregate_raster(), \n sheep_cluster()"
  )
}
