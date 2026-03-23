#' Custom ggplot2 theme for ecological figures
#'
#' Publication-style ggplot2 theme for ecology.
#'
#' A custom ggplot2 theme and helper layer bundle for producing
#' publication-style ecological figures. The function can control
#' legend layout, background style, grid lines, equal axis scaling,
#' reference lines, color and fill scales, and shaded x-axis regions.
#'
#' @param font Character string giving the base font family.
#' @param fontface Character string giving the font face for the plot title.
#' @param legend.title Character string or expression used as the legend title.
#' @param legend.ncol Number of legend columns when the legend is on the left or right.
#' @param legend.position Legend position. One of `"right"`, `"left"`, `"top"`, `"bottom"`, or `"none"`.
#' @param axis.text.x.angle Angle of x-axis text in degrees.
#' @param color Color scale option. One of `"discrete"`, `"gradient_earth"`, `"gradient_earth_discrete"`, `"spectral"`, or `"none"`.
#' @param fill Fill scale option. One of `"discrete"`, `"gradient_earth"`, `"gradient_earth_discrete"`, `"spectral"`, or `"none"`.
#' @param grid Grid line option. One of `"none"`, `"y"`, `"x"`, or `"both"`.
#' @param tight_axes Logical; if `TRUE`, removes padding between the data and axes.
#' @param background Background style. One of `"white"`, `"offwhite"`, or `"transparent"`.
#' @param equal_axes Logical; if `TRUE`, uses equal scaling on both axes.
#' @param one_to_one_line Logical; if `TRUE`, adds a one-to-one reference line.
#' @param hline Optional numeric value giving a horizontal reference line position.
#' @param vline Optional numeric value giving a vertical reference line position.
#' @param shade Optional numeric vector of even length. Each consecutive pair defines
#' a shaded x-axis interval spanning the full y-range of the plot.
#'
#' @return A list of ggplot2 theme, scale, guide, coordinate, annotation, and
#' reference-line components that can be added to a ggplot object.
#'
#' @examples
#' library(ggplot2)
#'
#' dat <- data.frame(
#'   x = rep(1:10, 2),
#'   y = c(1:10, 2:11),
#'   group = rep(c("A", "B"), each = 10)
#' )
#'
#' ggplot(dat, aes(x, y, color = group)) +
#'   geom_line() +
#'   theme_martin()
#'
#' ggplot(dat, aes(x, y, color = group)) +
#'   geom_line() +
#'   theme_martin(
#'     legend.position = "top",
#'     shade = c(3, 5, 7, 8)
#'   )
#'
#' @export

theme_martin <- function(
    font = "serif",
    fontface = "plain",
    legend.title = "Legend Title",
    legend.ncol = 1,
    legend.position = c("right",
                        "left",
                        "top",
                        "bottom",
                        "none"),
    axis.text.x.angle = 0,
    color = c("discrete",
              "gradient_earth",
              "gradient_earth_discrete",
              "spectral",
              "none"),
    fill = c("discrete",
             "gradient_earth",
             "gradient_earth_discrete",
             "spectral",
             "none"),
    grid = c("none",
             "y",
             "x",
             "both"),
    tight_axes = FALSE,
    background = c("white",
                   "offwhite",
                   "transparent"),
    equal_axes = FALSE,
    one_to_one_line = FALSE,
    hline = NULL,
    vline = NULL,
    shade = NULL
) {
  color <- match.arg(color)
  fill <- match.arg(fill)
  grid <- match.arg(grid)
  legend.position <- match.arg(legend.position)
  background <- match.arg(background)

  if (!is.null(shade)) {
    if (!is.numeric(shade) || length(shade) %% 2 != 0) {
      stop("shade must be NULL or a numeric vector with even length.")
    }
  }

  axis_lwd <- 1

  bg_fill <- switch(
    background,
    "transparent" = NA,
    "offwhite" = "#FCFBF7",
    "white" = "white"
  )

  strip_fill <- switch(
    background,
    "transparent" = "grey92",
    "offwhite" = "#F2EFE8",
    "white" = "grey92"
  )

  hjust_val <- if (axis.text.x.angle == 0) 0.5 else 1
  vjust_val <- if (axis.text.x.angle == 0) 0.5 else 1

  legend_direction <- if (legend.position %in% c("top", "bottom")) {
    "horizontal"
  } else {
    "vertical"
  }

  ggplot2::update_geom_defaults(
    "text",
    list(
      family = font,
      fontface = "plain",
      colour = rm_black
    )
  )

  ggplot2::update_geom_defaults(
    "label",
    list(
      family = font,
      fontface = "plain",
      colour = rm_black
    )
  )

  color_guide <- if (legend.position %in% c("top", "bottom")) {
    ggplot2::guide_legend(nrow = 1, byrow = TRUE)
  } else {
    ggplot2::guide_legend(ncol = legend.ncol, byrow = TRUE)
  }

  fill_guide <- if (legend.position %in% c("top", "bottom")) {
    ggplot2::guide_legend(nrow = 1, byrow = TRUE)
  } else {
    ggplot2::guide_legend(ncol = legend.ncol, byrow = TRUE)
  }

  base_theme <- ggplot2::theme_classic() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),

      plot.title = ggplot2::element_text(
        family = font,
        face = 'plain',
        size = 16,
        hjust = 0,
        vjust = 2,
        color = rm_black
      ),

      plot.subtitle = ggplot2::element_text(
        family = font,
        face = "plain",
        size = 12,
        hjust = 0,
        vjust = 2,
        color = rm_black
      ),

      plot.caption = ggplot2::element_text(
        family = font,
        face = "italic",
        size = 10,
        hjust = 0,
        margin = ggplot2::margin(t = 10),
        color = rm_black
      ),

      axis.title = ggplot2::element_text(
        family = font,
        face = 'plain',
        size = 16,
        hjust = 0.5,
        color = rm_black
      ),

      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 8)
      ),

      axis.text.x = ggplot2::element_text(
        family = font,
        face = 'plain',
        size = 11,
        angle = axis.text.x.angle,
        hjust = hjust_val,
        vjust = vjust_val,
        margin = ggplot2::margin(t = 3),
        color = rm_black
      ),

      axis.text.y = ggplot2::element_text(
        family = font,
        face = 'plain',
        size = 11,
        vjust = 0,
        margin = ggplot2::margin(r = 3),
        color = rm_black
      ),

      strip.text = ggplot2::element_text(
        family = font,
        face = fontface,
        size = 12,
        color = rm_black,
        margin = ggplot2::margin(t = 4, r = 4, b = 4, l = 4)
      ),
      # strip.placement = "outside",
      # strip.switch.pad.wrap = grid::unit(0, "pt"),
      # strip.switch.pad.grid = grid::unit(0, "pt"),
      # strip.background = ggplot2::element_blank(),
      strip.background.x = ggplot2::element_rect(
        fill = NA,
        colour = rm_black,
        linewidth = 0.7,
        linejoin = "bevel"
      ),
      strip.background.y = ggplot2::element_rect(
        fill = NA,
        colour = rm_black,
        linewidth = 1,
        linejoin = "bevel"
      ),

      legend.title = ggplot2::element_text(
        family = font,
        face = fontface,
        size = 11,
        hjust = 0.5,
        color = rm_black
      ),

      legend.text = ggplot2::element_text(
        family = font,
        face = "plain",
        size = 10,
        color = rm_black
      ),


      axis.ticks = ggplot2::element_line(
        color = rm_black,
        linewidth = axis_lwd
      ),

      panel.border = ggplot2::element_rect(
        color = rm_black,
        fill = NA,
        linewidth = 0.7,
        linejoin = "mitre",
        margin_auto(t = 0, r = t, b = t, l = r, unit = "pt")
      ),

      legend.position = legend.position,
      legend.direction = legend_direction,
      legend.title.align = 0.5,
      legend.key.width = grid::unit(1.2, "lines"),
      legend.key.height = grid::unit(1.2, "lines"),
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_rect(fill = NA, color = NA),

      plot.margin = ggplot2::margin(t = 10, r = 10, b = 10, l = 10),

      plot.background = ggplot2::element_rect(fill = bg_fill, color = NA),
      panel.background = ggplot2::element_rect(fill = bg_fill, color = NA)
    )

  grid_theme <- switch(
    grid,
    "none" = ggplot2::theme(),
    "y" = ggplot2::theme(
      panel.grid.major.y = ggplot2::element_line(color = "#D9D9D9", linewidth = 0.3)
    ),
    "x" = ggplot2::theme(
      panel.grid.major.x = ggplot2::element_line(color = "#D9D9D9", linewidth = 0.3)
    ),
    "both" = ggplot2::theme(
      panel.grid.major.x = ggplot2::element_line(color = "#D9D9D9", linewidth = 0.3),
      panel.grid.major.y = ggplot2::element_line(color = "#D9D9D9", linewidth = 0.3)
    )
  )

  scales <- list()

  if (color == "discrete") {
    scales <- c(scales, list(
      ggplot2::scale_color_manual(values = rm_discrete, name = legend.title)
    ))
  }

  if (color == "gradient_earth") {
    scales <- c(scales, list(
      ggplot2::scale_color_gradientn(colors = gradient_earth, name = legend.title)
    ))
  }

  if (color == "gradient_earth_discrete") {
    scales <- c(scales, list(
      ggplot2::scale_color_manual(values = gradient_earth, name = legend.title)
    ))
  }

  if (color == "spectral") {
    scales <- c(scales, list(
      ggplot2::scale_color_brewer(
        palette = "Spectral",
        direction = -1,
        name = legend.title
      )
    ))
  }

  if (fill == "discrete") {
    scales <- c(scales, list(
      ggplot2::scale_fill_manual(values = rm_discrete, name = legend.title)
    ))
  }

  if (fill == "gradient_earth") {
    scales <- c(scales, list(
      ggplot2::scale_fill_gradientn(colors = gradient_earth, name = legend.title)
    ))
  }

  if (fill == "gradient_earth_discrete") {
    scales <- c(scales, list(
      ggplot2::scale_fill_manual(values = gradient_earth, name = legend.title)
    ))
  }

  if (fill == "spectral") {
    scales <- c(scales, list(
      ggplot2::scale_fill_brewer(
        palette = "Spectral",
        direction = -1,
        name = legend.title
      )
    ))
  }

  guide_layer <- list(
    ggplot2::guides(
      color = color_guide,
      fill = fill_guide
    )
  )

  coord_layer <- if (equal_axes) {
    list(
      ggplot2::coord_fixed(
        ratio = 1,
        expand = !tight_axes,
        clip = "off"
      )
    )
  } else if (tight_axes) {
    list(
      ggplot2::coord_cartesian(
        expand = FALSE,
        clip = "off"
      )
    )
  } else {
    list(
      ggplot2::coord_cartesian(clip = "off")
    )
  }

  shade_layer <- list()

  if (!is.null(shade)) {
    shade_mat <- matrix(shade, ncol = 2, byrow = TRUE)

    shade_layer <- lapply(seq_len(nrow(shade_mat)), function(i) {
      xvals <- sort(shade_mat[i, ])

      ggplot2::annotate(
        "rect",
        xmin = xvals[1],
        xmax = xvals[2],
        ymin = -Inf,
        ymax = Inf,
        fill = "grey70",
        alpha = 0.25
      )
    })
  }

  ref_layers <- list()

  if (isTRUE(one_to_one_line)) {
    ref_layers <- c(ref_layers, list(
      ggplot2::geom_abline(
        slope = 1,
        intercept = 0,
        color = rm_indigo,
        linewidth = 0.75,
        linetype = "dotdash",
        inherit.aes = FALSE
      )
    ))
  }

  if (!is.null(hline)) {
    ref_layers <- c(ref_layers, list(
      ggplot2::geom_hline(
        yintercept = hline,
        color = rm_indigo,
        linewidth = 0.75,
        linetype = "dotdash"
      )
    ))
  }

  if (!is.null(vline)) {
    ref_layers <- c(ref_layers, list(
      ggplot2::geom_vline(
        xintercept = vline,
        color = rm_indigo,
        linewidth = 0.75,
        linetype = "dotdash"
      )
    ))
  }

  c(
    shade_layer,
    list(base_theme, grid_theme),
    scales,
    guide_layer,
    coord_layer,
    ref_layers
  )
}
