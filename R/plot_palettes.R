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
    ),
    temp = c(
      "#1B4F8C", "#4A85C2", "#9BB5C2", "#D4B59A",
      "#D87856", "#E25822", "#C41E13"
    )
  ),
  # discrete = c(
  #   "#2A3B28", "#D6A64C", "#2F2F6B", "#C24A3A",
  #   "#4E8F87", "#7A5A3A", "#8FA78F", "#E6D8A8"
  # ),
  discrete = c(
    '#EE7733', '#0077BB', '#2F2F6B', "#D6A64C", '#EE3377', '#CC3311', '#009988'
  ),
  discrete_plants = c(
    grass = "#E8B82E",
    shrub = "#C2451E",
    forb  = "#7AAE3A",
    tree  = "#0F2510",
    fern  = "#3F8579"
  ),
  plant_phenology = c(
    emergent = "#7ED957",
    flower   = "#2E5A2E",
    fruiting = "#B5C436",
    mature   = "#C99A1F",
    senesced = "#4A2E1A"
  ),
  gender = c(
    male    = "#2F2F6B",
    female  = "#B8336A",
    unknown = "#4A4A4A"
  ),
  season = c(
    spring = "#A8C97F",
    summer = "#D4A847",
    fall   = "#B5562A",
    winter = "#7A8FA6"
  )
)

# =========================
# VALID PALETTE NAMES
# =========================
.pal_choices <- c(
  # base + facet utilities
  "base", "white", "black", "grid", "sand", "teal",
  # discrete
  "discrete", "plants", "phenology", "gender", "season",
  # continuous gradients
  "temp", "earth", "cont",
  # ribbon (continuous, 3-stop)
  "ribbon_forest", "ribbon_teal"
)

.pal_discrete_names <- c(
  "discrete", "plants", "phenology", "gender", "season"
)

#' Get a Martin palette
#'
#' Single accessor for all palettes in the package.
#'
#' Valid `palette` values:
#' * Utility: `"base"` (named list of white/black/grid), `"white"`, `"black"`,
#'   `"grid"`, `"sand"`, `"teal"` (single hex strings)
#' * Discrete: `"discrete"`, `"plants"`, `"phenology"`, `"gender"`, `"season"`
#' * Continuous: `"temp"`, `"earth"`, `"cont"`
#' * Ribbon: `"ribbon_forest"`, `"ribbon_teal"` (3-stop named vectors)
#'
#' @param palette Palette name. See Details for valid values.
#' @param n Optional integer. When `palette = "discrete"`, returns the first
#'   `n` colors. Ignored for other palettes; supplying `n` for any palette
#'   other than `"discrete"` raises an error.
#'
#' @return A character vector of hex colors (named for `"plants"`,
#'   `"phenology"`, `"gender"`, `"season"`, and ribbon palettes), a single
#'   hex string for utility colors, or a list for `"base"`.
#' @export
pal <- function(palette, n = NULL) {
  palette <- match.arg(palette, .pal_choices)

  if (!is.null(n) && palette != "discrete") {
    stop(
      "`n` is only valid when `palette = \"discrete\"`.",
      call. = FALSE
    )
  }

  out <- switch(
    palette,
    # utilities
    base  = .martin_palettes$base,
    white = .martin_palettes$base$white,
    black = .martin_palettes$base$black,
    grid  = .martin_palettes$base$grid,
    sand  = .martin_palettes$facet$sand,
    teal  = .martin_palettes$facet$teal,
    # discrete
    discrete  = .martin_palettes$discrete,
    plants    = .martin_palettes$discrete_plants,
    phenology = .martin_palettes$plant_phenology,
    gender    = .martin_palettes$gender,
    season    = .martin_palettes$season,
    # continuous
    temp  = .martin_palettes$gradient$temp,
    earth = .martin_palettes$gradient$earth,
    cont  = .martin_palettes$gradient$cont,
    # ribbon
    ribbon_forest = .martin_palettes$ribbon$forest,
    ribbon_teal   = .martin_palettes$ribbon$teal
  )

  if (palette == "discrete" && !is.null(n)) {
    n <- as.integer(n)
    if (is.na(n) || n < 1 || n > length(out)) {
      stop(
        "`n` must be between 1 and ", length(out), ".",
        call. = FALSE
      )
    }
    out <- out[seq_len(n)]
  }

  out
}

#' Get interpolated continuous palette
#'
#' Returns `n` interpolated colors from the `"cont"` gradient.
#'
#' @param n Number of colors to interpolate.
#' @return A character vector of `n` hex colors.
#' @export
pal_continuous_n <- function(n = 256) {
  n <- as.integer(n)
  if (is.na(n) || n < 1) {
    stop("`n` must be a positive integer.", call. = FALSE)
  }
  grDevices::colorRampPalette(.martin_palettes$gradient$cont)(n)
}

