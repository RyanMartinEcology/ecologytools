# =========================
# INTERNAL PALETTE STORAGE
# =========================

.martin_palettes <- list(

  base = list(
    white = "#FFFFF0",
    black = "#1A1A14",
    grid  = "#D8D4C8"
  ),

  facet = list(
    teal = "#E3ECEB",
    sand = "#F3E8D0"
  ),

  ribbon = list(
    forest = c(
      outer = "#DDE5D8",
      mid   = "#7FA37C",
      inner = "#2F4F3A"
    ),
    teal = c(
      outer = "#DCE6E8",
      mid   = "#6F8F95",
      inner = "#2F4F54"
    )
  ),

  gradient = list(
    earth = c(
      "#3F6B35", "#567C40", "#6F8E4C", "#89A05A",
      "#A1A764", "#947A50", "#68482F", "#3B2218"
    ),
    cont = c(
      "#243B3B", "#2F5A5A", "#3F7A74", "#5F9A8C",
      "#87B09A", "#B7BE8A", "#D6C07A", "#F0E2B6"
    )
  ),

  discrete = c(
    "#2A3B28", "#D6A64C", "#2F2F6B", "#C24A3A",
    "#4E8F87", "#7A5A3A", "#8FA78F", "#E6D8A8"
  )
)

#' Get base colors
#' @export
pal_base <- function() {
  .martin_palettes$base
}

#' Get facet fill colors
#' @export
pal_facet <- function(type = c("teal", "sand")) {
  type <- match.arg(type)
  .martin_palettes$facet[[type]]
}

#' Get discrete palette
#' @export
pal_discrete <- function(n = NULL) {
  pal <- .martin_palettes$discrete

  if (is.null(n)) {
    return(pal)
  }

  n <- as.integer(n)
  if (is.na(n) || n < 1 || n > length(pal)) {
    stop("`n` must be between 1 and ", length(pal), ".", call. = FALSE)
  }

  pal[seq_len(n)]
}

#' Get ribbon palette
#' @export
pal_ribbon <- function(type = c("forest", "teal")) {
  type <- match.arg(type)
  .martin_palettes$ribbon[[type]]
}

#' Get gradient anchors
#' @export
pal_gradient <- function(type = c("earth", "cont")) {
  type <- match.arg(type)
  .martin_palettes$gradient[[type]]
}

#' Get interpolated continuous palette
#' @export
pal_continuous_n <- function(n = 256) {
  n <- as.integer(n)
  if (is.na(n) || n < 1) {
    stop("`n` must be a positive integer.", call. = FALSE)
  }

  grDevices::colorRampPalette(.martin_palettes$gradient$cont)(n)
}
