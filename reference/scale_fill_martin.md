# Martin fill scale

Applies a Martin palette to the `fill` aesthetic. Dispatches to
[`ggplot2::scale_fill_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html)
for discrete palettes and
[`ggplot2::scale_fill_gradientn()`](https://ggplot2.tidyverse.org/reference/scale_gradient.html)
for continuous palettes.

## Usage

``` r
scale_fill_martin(palette = "discrete")
```

## Arguments

- palette:

  One of `"discrete"`, `"plants"`, `"phenology"`, `"gender"`,
  `"season"`, `"temp"`, `"earth"`, `"cont"`, `"ribbon_forest"`, or
  `"ribbon_teal"`.

## Value

A ggplot2 scale.
