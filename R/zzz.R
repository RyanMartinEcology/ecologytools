.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "ecologytools ", utils::packageVersion("ecologytools"), "\n",
    "Utilities and Data Visualization Tools for Ecology\n",
    "Functions:\n aggregate_raster(),\n days_since_peak_IRG(),\n dist_escape(),\n force_inmemory(),\n fresh(),\n package_info(),\n pal(),\n pal_continuous_n(),\n plant_quality_summary(),\n raster_summary(),\n scale_color_martin(),\n scale_fill_martin(),\n sheep_cluster(),\n theme_martin(),\n time_it(),\n vrm(),\n vrml()"
  )
}
