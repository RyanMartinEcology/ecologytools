# Get a Martin palette

Single accessor for all palettes in the package.

## Usage

``` r
pal(palette, n = NULL)
```

## Arguments

- palette:

  Palette name. See Details for valid values.

- n:

  Optional integer. When `palette = "discrete"`, returns the first `n`
  colors. Ignored for other palettes; supplying `n` for any palette
  other than `"discrete"` raises an error.

## Value

A character vector of hex colors (named for `"plants"`, `"phenology"`,
`"gender"`, `"season"`, and ribbon palettes), a single hex string for
utility colors, or a list for `"base"`.

## Details

Valid `palette` values:

- Utility: `"base"` (named list of white/black/grid), `"white"`,
  `"black"`, `"grid"`, `"sand"`, `"teal"` (single hex strings)

- Discrete: `"discrete"`, `"plants"`, `"phenology"`, `"gender"`,
  `"season"`

- Continuous: `"temp"`, `"earth"`, `"cont"`

- Ribbon: `"ribbon_forest"`, `"ribbon_teal"` (3-stop named vectors)
