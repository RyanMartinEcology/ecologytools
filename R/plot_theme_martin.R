#' Martin theme
#'
#' A clean publication-style ggplot2 theme. Built on `theme_void()` for
#' predictable inheritance. Pair with `scale_color_martin()` /
#' `scale_fill_martin()` for matching palettes.
#'
#' @param base_size Base font size. If `NULL` (the default), resolves to 10
#'   when `publication = TRUE` and 12 otherwise.
#' @param base_family Base font family.
#' @param line_width Width of axis lines and ticks. Gridlines are drawn at
#'   half this value.
#' @param legend_position Legend position. One of `"right"` (default),
#'   `"left"`, `"top"`, or `"bottom"`.
#' @param facet_fill Facet strip fill. One of `"sand"`, `"teal"`, or `"none"`.
#' @param grid Gridline display. One of `"off"`, `"x"`, `"y"`, or `"xy"`.
#' @param full_box Logical. If `TRUE`, draw a full rectangular panel border
#'   with ticks on all four sides (tick labels remain on bottom and left only).
#' @param axis_text_x_angle Rotation angle for x-axis text labels.
#' @param publication Logical. If `TRUE`, applies publication-ready defaults:
#'   `base_size` becomes 10 (unless supplied), `base_family` becomes
#'   `"Times New Roman"` (unless the user has changed it from the default),
#'   and the palette's off-black is replaced with pure black (`"#000000"`).
#'
#' @return A ggplot2 theme object.
#' @export
theme_martin <- function(
    base_size = NULL,
    base_family = "Libre Caslon Text",
    line_width = 1.1,
    legend_position = c("right", "left", "top", "bottom"),
    facet_fill = c("sand", "teal", "none"),
    grid = c("off", "x", "y", "xy"),
    full_box = FALSE,
    axis_text_x_angle = 0,
    publication = FALSE
) {
  legend_position <- match.arg(legend_position)
  facet_fill <- match.arg(facet_fill)
  grid <- match.arg(grid)

  # ---- resolve publication-dependent defaults ----
  if (is.null(base_size)) {
    base_size <- if (publication) 10 else 12
  }
  if (publication && identical(base_family, "Libre Caslon Text")) {
    base_family <- "Times New Roman"
  }

  base_cols <- pal("base")
  if (publication) {
    base_cols$black <- "#000000"
  }

  # ---- helper values ----
  half_line <- base_size / 2
  facet_col <- switch(
    facet_fill,
    sand = pal("sand"),
    teal = pal("teal"),
    none = base_cols$white
  )

  # ---- gridline visibility ----
  show_grid_x <- grid %in% c("x", "xy")
  show_grid_y <- grid %in% c("y", "xy")

  # ---- x-axis text rotation ----
  x_hjust <- if (axis_text_x_angle == 0) 0.5 else 1
  x_vjust <- if (axis_text_x_angle == 0) 0.5 else 1

  # ---- element constructors ----
  blk_line <- function(lw = line_width) {
    ggplot2::element_line(colour = base_cols$black, linewidth = lw)
  }

  ggplot2::theme_void(
    base_size   = base_size,
    base_family = base_family
  ) +
    ggplot2::theme(
      # ---- root inheritance ----
      text = ggplot2::element_text(
        family = base_family,
        size   = base_size,
        colour = base_cols$black
      ),
      line = blk_line(),

      # ---- plot-level ----
      plot.background  = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      plot.margin      = ggplot2::margin(half_line, half_line, half_line, half_line),

      plot.title = ggplot2::element_text(
        size   = base_size + 3,
        face   = "bold",
        hjust  = 0,
        colour = base_cols$black,
        margin = ggplot2::margin(b = half_line)
      ),
      plot.subtitle = ggplot2::element_text(
        size   = base_size - 1,
        face   = "italic",
        hjust  = 0,
        colour = base_cols$black,
        margin = ggplot2::margin(b = half_line)
      ),

      # ---- axes ----
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(t = half_line)
      ),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(r = half_line),
        angle  = 90
      ),
      axis.text.x = ggplot2::element_text(
        colour = base_cols$black,
        angle  = axis_text_x_angle,
        hjust  = x_hjust,
        vjust  = x_vjust,
        margin = ggplot2::margin(t = half_line / 2)
      ),
      axis.text.y = ggplot2::element_text(
        colour = base_cols$black,
        margin = ggplot2::margin(r = half_line / 2)
      ),

      # axis lines: bottom/left only when full_box = FALSE; suppressed when TRUE
      # (panel.border takes over to avoid double-drawing on bottom and left edges)
      axis.line.x.bottom = if (full_box) ggplot2::element_blank() else blk_line(),
      axis.line.y.left   = if (full_box) ggplot2::element_blank() else blk_line(),
      axis.line.x.top    = ggplot2::element_blank(),
      axis.line.y.right  = ggplot2::element_blank(),

      # ticks: always on bottom/left; on top/right only when full_box = TRUE
      axis.ticks.x.bottom = blk_line(),
      axis.ticks.y.left   = blk_line(),
      axis.ticks.x.top    = if (full_box) blk_line() else ggplot2::element_blank(),
      axis.ticks.y.right  = if (full_box) blk_line() else ggplot2::element_blank(),
      axis.ticks.length   = grid::unit(3, "pt"),

      # ---- panel border ----
      panel.border = if (full_box) {
        ggplot2::element_rect(
          fill      = NA,
          colour    = base_cols$black,
          linewidth = line_width
        )
      } else {
        ggplot2::element_blank()
      },

      # ---- gridlines ----
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = if (show_grid_x) {
        ggplot2::element_line(
          colour    = base_cols$grid,
          linewidth = line_width / 2
        )
      } else {
        ggplot2::element_blank()
      },
      panel.grid.major.y = if (show_grid_y) {
        ggplot2::element_line(
          colour    = base_cols$grid,
          linewidth = line_width / 2
        )
      } else {
        ggplot2::element_blank()
      },

      # ---- legend ----
      legend.position    = legend_position,
      legend.background  = ggplot2::element_blank(),
      legend.key         = ggplot2::element_blank(),
      legend.title       = ggplot2::element_text(
        colour = base_cols$black,
        face   = "plain",
        hjust  = 0.5
      ),
      legend.title.align = 0.5,
      legend.text        = ggplot2::element_text(colour = base_cols$black),

      # ---- facet strips ----
      panel.spacing = grid::unit(0.5, "lines"),
      strip.background = ggplot2::element_rect(
        fill   = facet_col,
        colour = NA
      ),
      strip.text = ggplot2::element_text(
        colour = base_cols$black,
        face   = "bold",
        margin = ggplot2::margin(half_line / 2, half_line / 2, half_line / 2, half_line / 2)
      )
    )
}


# =========================
# INTERNAL: scale dispatch
# =========================
.martin_scale_choices <- c(
  "discrete", "plants", "phenology", "gender", "season",
  "temp", "earth", "cont", "ribbon_forest", "ribbon_teal"
)

.martin_scale_is_continuous <- function(palette) {
  palette %in% c("temp", "earth", "cont", "ribbon_forest", "ribbon_teal")
}


#' Martin color scale
#'
#' Applies a Martin palette to the `color` aesthetic. Dispatches to
#' `ggplot2::scale_color_manual()` for discrete palettes and
#' `ggplot2::scale_color_gradientn()` for continuous palettes.
#'
#' @param palette One of `"discrete"`, `"plants"`, `"phenology"`, `"gender"`,
#'   `"season"`, `"temp"`, `"earth"`, `"cont"`, `"ribbon_forest"`, or
#'   `"ribbon_teal"`.
#'
#' @return A ggplot2 scale.
#' @export
scale_color_martin <- function(palette = "discrete") {
  palette <- match.arg(palette, .martin_scale_choices)
  values <- unname(pal(palette))
  if (.martin_scale_is_continuous(palette)) {
    ggplot2::scale_color_gradientn(colors = values)
  } else {
    ggplot2::scale_color_manual(values = pal(palette))
  }
}

#' Martin fill scale
#'
#' Applies a Martin palette to the `fill` aesthetic. Dispatches to
#' `ggplot2::scale_fill_manual()` for discrete palettes and
#' `ggplot2::scale_fill_gradientn()` for continuous palettes.
#'
#' @param palette One of `"discrete"`, `"plants"`, `"phenology"`, `"gender"`,
#'   `"season"`, `"temp"`, `"earth"`, `"cont"`, `"ribbon_forest"`, or
#'   `"ribbon_teal"`.
#'
#' @return A ggplot2 scale.
#' @export
scale_fill_martin <- function(palette = "discrete") {
  palette <- match.arg(palette, .martin_scale_choices)
  values <- unname(pal(palette))
  if (.martin_scale_is_continuous(palette)) {
    ggplot2::scale_fill_gradientn(colors = values)
  } else {
    ggplot2::scale_fill_manual(values = pal(palette))
  }
}


