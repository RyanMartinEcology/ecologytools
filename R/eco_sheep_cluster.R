#' Cluster recent sheep GPS locations into search areas
#'
#' Creates recent last locations, movement paths, and DBSCAN-based convex hull
#' search areas from GPS data. Optionally writes outputs as KML or GPX and
#' produces a basic plot.
#'
#' @importFrom rlang .data
#'
#' @param GPS A data frame containing columns `x`, `y`, `ID`, and `DateTime`.
#'   `x` and `y` should be projected coordinates in the supplied `crs`.
#'   `DateTime` must be a `POSIXct` vector.
#' @param no.days Numeric. Number of days before `reference_date` to retain.
#' @param reference_date A `POSIXct` reference date. Defaults to current time.
#' @param minimum.points Integer. Minimum number of points required by DBSCAN
#'   to define a cluster.
#' @param epsilon Numeric. DBSCAN epsilon parameter, in map units of `crs`.
#' @param crs Character string giving the coordinate reference system, passed to
#'   [terra::vect()]. Defaults to `"EPSG:32612"`.
#' @param filetype Character. Output vector file type. Must be either `"KML"`
#'   or `"GPX"`.
#' @param last.point Logical. If `TRUE`, write last locations for each
#'   individual.
#' @param trace Logical. If `TRUE`, write movement paths for each individual.
#' @param basic.plot Logical. If `TRUE`, draw a simple plot of search areas,
#'   movement paths, and last locations.
#'
#' @return A `terra::SpatVector` of search areas if clusters are found. If no
#'   clusters are found, returns the point `SpatVector` with attached
#'   `clusterID`.
#'
#' @details
#' The function filters GPS locations to observations occurring within
#' `no.days` before `reference_date`, then applies DBSCAN clustering to all
#' retained points. Minimum convex hulls are drawn around non-noise clusters.
#'
#' Output files are written to the working directory as:
#' - `last_locations.<ext>`
#' - `movement_paths.<ext>`
#' - `search_areas.<ext>`
#'
#' where `<ext>` is determined by `filetype`.
#'
#' GPX support for polygon outputs depends on the software used to read the
#' file, and is generally less robust than KML for search-area polygons.
#'
#' @examples
#' \dontrun{
#' out <- sheep_cluster(
#'   GPS = gps_df,
#'   no.days = 7,
#'   minimum.points = 10,
#'   epsilon = 100,
#'   crs = "EPSG:32612",
#'   filetype = "KML"
#' )
#' }
#'
#' @export

sheep_cluster <- function(GPS,
                          no.days = 7,
                          reference_date = Sys.time(),
                          minimum.points = 10,
                          epsilon = 100,
                          crs = 'EPSG:32612',
                          filetype = 'KML',
                          last.point = T,
                          trace = T,
                          basic.plot = F) {

  # ----------------------------------------------------------------------------------------------------------------------
  # validate inputs
  # ----------------------------------------------------------------------------------------------------------------------

  #1) require the expected columns
  required_cols <- c('x', 'y', 'ID', 'DateTime')
  missing_cols <- setdiff(x = required_cols, y = names(GPS))
  if (length(missing_cols) > 0) {
    stop(
      'GPS is missing required columns: ',
      paste(missing_cols, collapse = ', '),
      call. = F
    )
  }

  #2) require a POSIXct datetime column
  if (!inherits(x = GPS$DateTime, what = 'POSIXct')) {
    stop('GPS$DateTime must be a POSIXct column.', call. = F)
  }

  #3) coerce the reference date to POSIXct
  if (!inherits(x = reference_date, what = 'POSIXct')) {
    reference_date <- as.POSIXct(reference_date)
  }

  #4) require a usable reference date
  if (is.na(reference_date)) {
    stop('reference_date could not be converted to POSIXct.', call. = F)
  }

  #5) require a supported output file type
  filetype <- toupper(filetype)
  if (!filetype %in% c('KML', 'GPX')) {
    stop("filetype must be either 'KML' or 'GPX'.", call. = F)
  }

  #6) derive the output file extension
  file_ext <- tolower(filetype)

  # ----------------------------------------------------------------------------------------------------------------------
  # prepare and filter points
  # ----------------------------------------------------------------------------------------------------------------------

  #1) coerce columns and compute time since reference
  GPS <- dplyr::mutate(
    GPS,
    x = as.numeric(.data$x),
    y = as.numeric(.data$y),
    ID = as.character(.data$ID),
    Date = .data$DateTime,
    Time.Difference = reference_date - .data$Date
  )

  #2) keep observations within no.days before the reference date
  GPS_window <- dplyr::filter(
    GPS,
    .data$Time.Difference >= 0,
    .data$Time.Difference <= lubridate::days(no.days)
  )

  #3) require at least one retained point
  if (nrow(GPS_window) == 0) {
    stop(
      'No points remain after filtering by no.days and reference_date.',
      call. = F
    )
  }

  #4) initialize optional outputs
  last.points <- NULL
  trace.out <- NULL

  # ----------------------------------------------------------------------------------------------------------------------
  # last locations
  # ----------------------------------------------------------------------------------------------------------------------

  #1) build, vectorize, and write last locations per individual
  if (isTRUE(last.point)) {
    last.points_df <- GPS_window |>
      dplyr::group_by(.data$ID) |>
      dplyr::filter(.data$Date == max(.data$Date, na.rm = T)) |>
      dplyr::ungroup() |>
      dplyr::mutate(animalID = .data$ID) |>
      dplyr::select(.data$x, .data$y, .data$animalID)

    last.points <- terra::vect(
      x = last.points_df,
      geom = c('x', 'y'),
      crs = crs
    )

    terra::writeVector(
      x = last.points,
      filename = paste0('last_locations.', file_ext),
      filetype = filetype,
      overwrite = T
    )
  }

  # ----------------------------------------------------------------------------------------------------------------------
  # movement traces
  # ----------------------------------------------------------------------------------------------------------------------

  #1) build per-individual movement lines and write them
  if (isTRUE(trace)) {
    trace_df <- GPS_window |>
      dplyr::mutate(animalID = .data$ID) |>
      dplyr::select(.data$x, .data$y, .data$animalID, .data$Date)

    trace_vect <- terra::vect(
      x = trace_df,
      geom = c('x', 'y'),
      crs = crs
    )

    animal <- unique(trace_vect$animalID)
    trace.list <- vector(mode = 'list', length = length(animal))

    for (i in seq_along(animal)) {
      ind.trace <- trace_vect[trace_vect$animalID == animal[i], ]
      ind.trace <- ind.trace[order(ind.trace$Date), ]
      trace.list[[i]] <- terra::as.lines(ind.trace)
    }

    trace.out <- do.call(what = rbind, args = trace.list)
    trace.out$ID <- animal

    terra::writeVector(
      x = trace.out,
      filename = paste0('movement_paths.', file_ext),
      filetype = filetype,
      overwrite = T
    )
  }

  # ----------------------------------------------------------------------------------------------------------------------
  # cluster points
  # ----------------------------------------------------------------------------------------------------------------------

  #1) order points for clustering and vectorize
  GPS_clust <- GPS_window |>
    dplyr::select(
      x = .data$x,
      y = .data$y,
      datetime = .data$Date,
      animalID = .data$ID
    ) |>
    dplyr::arrange(.data$animalID, .data$datetime)

  points <- terra::vect(
    x = GPS_clust,
    geom = c('x', 'y'),
    crs = crs
  )

  #2) run DBSCAN on the coordinate matrix
  points.matrix <- cbind(GPS_clust$x, GPS_clust$y)

  cluster_out <- dbscan::dbscan(
    x = points.matrix,
    eps = epsilon,
    minPts = minimum.points
  )

  #3) attach cluster ids and drop noise
  points$clusterID <- cluster_out$cluster

  cluster_ids <- sort(unique(points$clusterID))
  cluster_ids <- cluster_ids[cluster_ids > 0]

  # ----------------------------------------------------------------------------------------------------------------------
  # assemble search areas and plot
  # ----------------------------------------------------------------------------------------------------------------------

  #1) handle the no-cluster case, optionally plotting, and return points
  if (length(cluster_ids) == 0) {
    message('No clusters found.')

    if (isTRUE(basic.plot)) {
      plotted <- F

      if (!is.null(last.points)) {
        terra::plot(
          x = last.points,
          col = 'red',
          pch = 16,
          main = 'sheep_cluster outputs'
        )
        plotted <- T
      }

      if (!is.null(trace.out)) {
        if (!plotted) {
          terra::plot(
            x = trace.out,
            col = 'blue',
            lwd = 2,
            main = 'sheep_cluster outputs'
          )
        } else {
          terra::plot(x = trace.out, col = 'blue', lwd = 2, add = T)
        }
      }

      graphics::legend(
        x = 'topright',
        legend = c('Last locations', 'Movement paths'),
        col = c('red', 'blue'),
        pch = c(16, NA),
        lty = c(NA, 1),
        lwd = c(NA, 2),
        bty = 'n'
      )
    }

    return(points)
  }

  #2) draw a convex hull around each non-noise cluster
  mcp.list <- vector(mode = 'list', length = length(cluster_ids))
  for (i in seq_along(cluster_ids)) {
    cluster.points <- points[points$clusterID == cluster_ids[i], ]
    mcp <- terra::convHull(cluster.points)
    mcp$clusterID <- cluster_ids[i]
    mcp.list[[i]] <- mcp
  }

  #3) combine the hulls and write the search areas
  search.areas <- do.call(what = rbind, args = mcp.list)

  terra::writeVector(
    x = search.areas,
    filename = paste0('search_areas.', file_ext),
    filetype = filetype,
    overwrite = T
  )

  #4) optionally plot search areas with traces and last locations
  if (isTRUE(basic.plot)) {
    terra::plot(
      x = search.areas,
      col = 'lightblue',
      border = 'blue',
      lwd = 2,
      main = 'sheep_cluster outputs'
    )

    if (!is.null(trace.out)) {
      terra::plot(x = trace.out, col = 'darkgreen', lwd = 2, add = T)
    }

    if (!is.null(last.points)) {
      terra::plot(x = last.points, col = 'red', pch = 16, add = T)
    }

    graphics::legend(
      x = 'topright',
      legend = c('Search areas', 'Movement paths', 'Last locations'),
      fill = c('lightblue', NA, NA),
      border = c('blue', NA, NA),
      col = c('blue', 'darkgreen', 'red'),
      lty = c(NA, 1, NA),
      lwd = c(NA, 2, NA),
      pch = c(NA, NA, 16),
      bty = 'n'
    )
  }

  #5) return the assembled search areas
  search.areas
}
