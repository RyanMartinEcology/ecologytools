# Martin theme

A clean publication-style ggplot2 theme. Built on
[`theme_void()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
for predictable inheritance. Pair with
[`scale_color_martin()`](https://ryanmartinecology.github.io/ecologytools/reference/scale_color_martin.md)
/
[`scale_fill_martin()`](https://ryanmartinecology.github.io/ecologytools/reference/scale_fill_martin.md)
for matching palettes.

## Usage

``` r
theme_martin(
  base_size = NULL,
  base_family = "Libre Caslon Text",
  line_width = 1.1,
  legend_position = c("right", "left", "top", "bottom"),
  facet_fill = c("sand", "teal", "none"),
  grid = c("off", "x", "y", "xy"),
  full_box = F,
  axis_text_x_angle = 0,
  publication = F
)
```

## Arguments

- base_size:

  Base font size. If `NULL` (the default), resolves to 10 when
  `publication = TRUE` and 12 otherwise.

- base_family:

  Base font family.

- line_width:

  Width of axis lines and ticks. Gridlines are drawn at half this value.

- legend_position:

  Legend position. One of `"right"` (default), `"left"`, `"top"`, or
  `"bottom"`.

- facet_fill:

  Facet strip fill. One of `"sand"`, `"teal"`, or `"none"`.

- grid:

  Gridline display. One of `"off"`, `"x"`, `"y"`, or `"xy"`.

- full_box:

  Logical. If `TRUE`, draw a full rectangular panel border with ticks on
  all four sides (tick labels remain on bottom and left only).

- axis_text_x_angle:

  Rotation angle for x-axis text labels.

- publication:

  Logical. If `TRUE`, applies publication-ready defaults: `base_size`
  becomes 10 (unless supplied), `base_family` becomes
  `"Times New Roman"` (unless the user has changed it from the default),
  and the palette's off-black is replaced with pure black (`"#000000"`).

## Value

A ggplot2 theme object.
