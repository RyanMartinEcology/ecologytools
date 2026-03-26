#' Martin theme
#'
#' A clean publication-style ggplot2 theme with integrated palette support.
#'
#' @param base_size Base font size.
#' @param base_family Base font family.
#' @param palette Palette choice. One of `"discrete"`, `"cont_grad"`,
#'   `"grad_earth"`, `"ribbon_forest"`, or `"ribbon_teal"`.
#' @param palette_aes Aesthetic to which the palette is applied. One of
#'   `"fill"`, `"color"`, or `"both"`.
#' @param legend_title Optional legend title.
#' @param facet_fill Facet strip fill. One of `"sand"`, `"teal"`, or `"none"`.
#' @param grid Gridline display. One of `"off"`, `"x"`, `"y"`, or `"xy"`.
#' @param full_box Logical. If `TRUE`, draw top and right axis lines to complete
#'   the panel box.
#' @param axis_text_x_angle Rotation angle for x-axis text labels.
#'
#' @return A list of ggplot2 components.
#' @export
theme_martin <- function(
    base_size = 12,
    base_family = "Libre Caslon Text",
    palette = c("discrete", "cont_grad", "grad_earth", "ribbon_forest", "ribbon_teal"),
    palette_aes = c("fill", "color", "both"),
    legend_title = NULL,
    facet_fill = c("sand", "teal", "none"),
    grid = c("off", "x", "y", "xy"),
    full_box = FALSE,
    axis_text_x_angle = 0
) {
  palette <- match.arg(palette)
  palette_aes <- match.arg(palette_aes)
  facet_fill <- match.arg(facet_fill)
  grid <- match.arg(grid)

  base_cols <- pal_base()

  facet_col <- switch(
    facet_fill,
    sand = pal_facet("sand"),
    teal = pal_facet("teal"),
    none = base_cols$white
  )

  pal_vals <- switch(
    palette,
    discrete = pal_discrete(),
    cont_grad = pal_gradient("cont"),
    grad_earth = pal_gradient("earth"),
    ribbon_forest = unname(pal_ribbon("forest")),
    ribbon_teal = unname(pal_ribbon("teal"))
  )

  make_scale_layers <- function(values, aes, continuous) {
    if (continuous) {
      if (aes == "fill") {
        return(list(ggplot2::scale_fill_gradientn(colors = values)))
      }
      if (aes == "color") {
        return(list(ggplot2::scale_color_gradientn(colors = values)))
      }
      return(list(
        ggplot2::scale_fill_gradientn(colors = values),
        ggplot2::scale_color_gradientn(colors = values)
      ))
    }

    if (aes == "fill") {
      return(list(ggplot2::scale_fill_manual(values = values)))
    }
    if (aes == "color") {
      return(list(ggplot2::scale_color_manual(values = values)))
    }
    list(
      ggplot2::scale_fill_manual(values = values),
      ggplot2::scale_color_manual(values = values)
    )
  }

  scale_layers <- make_scale_layers(
    values = pal_vals,
    aes = palette_aes,
    continuous = palette %in% c("cont_grad", "grad_earth")
  )

  legend_layer <- if (is.null(legend_title)) {
    list()
  } else if (palette_aes == "fill") {
    list(ggplot2::labs(fill = legend_title))
  } else if (palette_aes == "color") {
    list(ggplot2::labs(color = legend_title))
  } else {
    list(ggplot2::labs(fill = legend_title, color = legend_title))
  }

  show_grid_x <- grid %in% c("x", "xy")
  show_grid_y <- grid %in% c("y", "xy")

  x_hjust <- if (axis_text_x_angle == 0) 0.5 else 1
  x_vjust <- if (axis_text_x_angle == 0) 0.5 else 1

  theme_layer <- list(
    ggplot2::theme_minimal(
      base_size = base_size,
      base_family = base_family
    ) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          size = base_size + 3,
          face = "bold",
          hjust = 0,
          colour = base_cols$black
        ),
        plot.subtitle = ggplot2::element_text(
          size = base_size - 1,
          face = "italic",
          hjust = 0,
          colour = base_cols$black
        ),
        legend.title = ggplot2::element_text(
          colour = base_cols$black,
          face = "plain",
          hjust = 0.5
        ),
        legend.title.align = 0.5,
        legend.text = ggplot2::element_text(
          colour = base_cols$black
        ),
        axis.text.x = ggplot2::element_text(
          colour = base_cols$black,
          angle = axis_text_x_angle,
          hjust = x_hjust,
          vjust = x_vjust
        ),
        axis.text.y = ggplot2::element_text(
          colour = base_cols$black
        ),
        axis.title = ggplot2::element_text(
          colour = base_cols$black
        ),
        axis.line = ggplot2::element_line(
          colour = base_cols$black,
          linewidth = 0.7
        ),
        axis.line.x.top = if (full_box) {
          ggplot2::element_line(colour = base_cols$black, linewidth = 0.7)
        } else {
          ggplot2::element_blank()
        },
        axis.line.y.right = if (full_box) {
          ggplot2::element_line(colour = base_cols$black, linewidth = 0.7)
        } else {
          ggplot2::element_blank()
        },
        axis.ticks = ggplot2::element_line(
          colour = base_cols$black,
          linewidth = 0.7
        ),
        axis.ticks.length = grid::unit(3, "pt"),
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.x = if (show_grid_x) {
          ggplot2::element_line(
            colour = base_cols$grid,
            linewidth = 0.4
          )
        } else {
          ggplot2::element_blank()
        },
        panel.grid.major.y = if (show_grid_y) {
          ggplot2::element_line(
            colour = base_cols$grid,
            linewidth = 0.4
          )
        } else {
          ggplot2::element_blank()
        },
        panel.spacing = grid::unit(1, "lines"),
        strip.background = ggplot2::element_rect(
          fill = facet_col,
          colour = NA
        ),
        strip.text = ggplot2::element_text(
          colour = base_cols$black,
          face = "bold"
        )
      )
  )

  c(theme_layer, legend_layer, scale_layers)
}
