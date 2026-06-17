# ecologytools

R utilities for ecology, centered on the `fresh()` linear-programming model of
habitat-scale forage resources for generalist herbivores and a suite of ggplot2
plotting tools — the `theme_martin()` publication theme with custom palettes and
scales. Also includes `terra`-based raster and terrain utilities, DBSCAN
clustering of GPS locations, and forage/diet-quality summaries.

License: GPL-3

# Installation Instructions

## remotes Package Installation

```r
install.packages("remotes")
remotes::install_github("RyanMartinEcology/ecologytools")
library(ecologytools)
```

## devtools Package Installation

```r
install.packages("devtools")
devtools::install_github("RyanMartinEcology/ecologytools")
library(ecologytools)
```

# FRESH: Forage Resource Evaluation [fresh()]

`fresh()` computes the maximum suitable forage biomass that meets minimum
digestible energy (DE) and digestible protein (DP) concentration constraints for
a generalist herbivore, using linear programming. One LP is solved per
`patch_id`. It returns a `fresh_output` object with a `$summary` (one row per
patch x constraint pair), a `$detail` table (per-forage contributions), and,
when a `habitat_type` column is supplied, a `$habitat` summary.

```r
library(ecologytools)

#---------------------------------------
# 1. Minimal single-constraint example
#---------------------------------------
# Forage biomass: one row per forage per patch (grams over a sampled area)
biomass_data <- data.frame(
  patch_id = c("A", "A", "A", "B", "B", "B"),
  plant_id = c("Vaccinium", "Carex", "Salix", "Vaccinium", "Carex", "Salix"),
  biomass  = c(120, 80, 45, 60, 150, 30),
  area     = 900
)

# Nutritional values: one row per plant_id
nutrition_data <- data.frame(
  plant_id        = c("Vaccinium", "Carex", "Salix"),
  de              = c(12.5, 9.8, 11.2),   # kJ/g
  dp              = c(9.0, 5.5, 8.1),     # g / 100 g
  diet_proportion = c(0.5, 0.3, 0.2)
)

result <- fresh(
  biomass_data   = biomass_data,
  nutrition_data = nutrition_data,
  animal_de_req  = 11.5,   # minimum diet DE (kJ/g)
  animal_dp_req  = 7.5,    # minimum diet DP (g / 100 g)
  animal_dmi     = 1500    # daily dry matter intake (g/day)
)

result           # S3 print method: quick FRESH summary
result$summary   # suitable biomass, animal-days, limiting constraint, etc.
result$detail    # biomass used and proportions per forage

#---------------------------------------
# 2. Multiple constraint pairs at once
#---------------------------------------
# Passing vectors solves each (de_req, dp_req) pair as a separate scenario.
multi <- fresh(
  biomass_data   = biomass_data,
  nutrition_data = nutrition_data,
  animal_de_req  = c(10.5, 11.5, 12.5),
  animal_dp_req  = c(6.0, 7.5, 9.0)
)
multi$summary
```

# Plot Utility Examples

```r
library(ggplot2)
library(ecologytools)

#---------------------------------------
# 1. Scatter plot with many colors
#---------------------------------------
set.seed(1)
df_point <- data.frame(
  x = rnorm(200),
  y = rnorm(200),
  group = sample(letters[1:7], 200, replace = TRUE)
)
ggplot(df_point, aes(x, y, color = group)) +
  geom_point(size = 2) +
  scale_color_martin("discrete") +
  theme_martin()

#---------------------------------------
# 2. Line plot with a continuous gradient
#---------------------------------------
df_line <- data.frame(
  x = rep(1:50, 6),
  y = c(
    cumsum(rnorm(50)),
    cumsum(rnorm(50)),
    cumsum(rnorm(50)),
    cumsum(rnorm(50)),
    cumsum(rnorm(50)),
    cumsum(rnorm(50))
  ),
  group = rep(1:6, each = 50)
)
ggplot(df_line, aes(x, y, color = group, group = group)) +
  geom_line(linewidth = 1) +
  scale_color_martin("earth") +
  theme_martin()

#---------------------------------------
# 3. Facet wrap example
#---------------------------------------
df_facet <- data.frame(
  x = rep(1:20, 4),
  y = rnorm(80),
  group = rep(letters[1:4], each = 20)
)
ggplot(df_facet, aes(x, y)) +
  geom_line(color = pal("black"), linewidth = 1) +
  facet_wrap(~group) +
  theme_martin()

#---------------------------------------
# 4. Plant functional groups
#---------------------------------------
df_plants <- data.frame(
  group = factor(
    c("grass", "shrub", "forb", "tree", "fern"),
    levels = c("grass", "shrub", "forb", "tree", "fern")
  ),
  cover = c(45, 22, 18, 10, 5)
)
ggplot(df_plants, aes(x = group, y = cover, fill = group)) +
  geom_col() +
  scale_fill_martin("plants") +
  theme_martin() +
  theme(legend.position = "none")

#---------------------------------------
# 5. Plant phenology
#---------------------------------------
df_phen <- data.frame(
  stage = factor(
    rep(c("emergent", "flower", "fruiting", "mature", "senesced"), each = 20),
    levels = c("emergent", "flower", "fruiting", "mature", "senesced")
  ),
  biomass = c(
    rnorm(20, 5, 1),
    rnorm(20, 12, 2),
    rnorm(20, 15, 2),
    rnorm(20, 10, 2),
    rnorm(20, 3, 1)
  )
)
ggplot(df_phen, aes(x = stage, y = biomass, fill = stage)) +
  geom_boxplot() +
  scale_fill_martin("phenology") +
  theme_martin() +
  theme(legend.position = "none")

#---------------------------------------
# 6. Season
#---------------------------------------
df_season <- data.frame(
  doy = rep(1:90, 4),
  ndvi = c(
    rnorm(90, 0.3, 0.05),
    rnorm(90, 0.7, 0.05),
    rnorm(90, 0.5, 0.05),
    rnorm(90, 0.2, 0.05)
  ),
  season = factor(
    rep(c("spring", "summer", "fall", "winter"), each = 90),
    levels = c("spring", "summer", "fall", "winter")
  )
)
ggplot(df_season, aes(x = doy, y = ndvi, color = season)) +
  geom_point() +
  scale_color_martin("season") +
  theme_martin()

#---------------------------------------
# 7. Gender
#---------------------------------------
df_gender <- data.frame(
  mass = c(rnorm(40, 75, 8), rnorm(40, 60, 7), rnorm(10, 65, 10)),
  sex = factor(
    c(rep("male", 40), rep("female", 40), rep("unknown", 10)),
    levels = c("male", "female", "unknown")
  )
)
ggplot(df_gender, aes(x = sex, y = mass, fill = sex)) +
  geom_violin() +
  scale_fill_martin("gender") +
  theme_martin() +
  theme(legend.position = "none")

#---------------------------------------
# 8. Temperature gradient
#---------------------------------------
df_temp <- expand.grid(x = 1:20, y = 1:20)
df_temp$temp <- with(df_temp, sin(x / 3) + cos(y / 3) + rnorm(nrow(df_temp), 0, 0.2))
ggplot(df_temp, aes(x, y, fill = temp)) +
  geom_tile() +
  scale_fill_martin("temp") +
  theme_martin()
```

## Palette Accessors [pal(), pal_continuous_n()]

```r
library(ecologytools)

pal("discrete")        # full discrete palette (hex vector)
pal("discrete", n = 3) # first 3 discrete colors
pal("plants")          # named functional-group colors
pal("base")            # list of white / black / grid utility colors
pal("temp")            # a continuous gradient's stops

pal_continuous_n(256)  # 256 colors interpolated from the "cont" gradient
```

# Spatial Utilities

## Distance to Escape Terrain [dist_escape()]

```r
library(terra)
library(ecologytools)
# Create a smooth synthetic elevation surface
r <- rast(nrows = 100, ncols = 100, xmin = 0, xmax = 100, ymin = 0, ymax = 100)
xy <- as.data.frame(crds(r))
z <- with(xy,
  800 +
    250 * exp(-((x - 30)^2 + (y - 35)^2) / 250) +
    180 * exp(-((x - 70)^2 + (y - 65)^2) / 180) -
    120 * exp(-((x - 55)^2 + (y - 45)^2) / 120) +
    60 * sin(x / 8) * cos(y / 10)
)
values(r) <- z
d <- dist_escape(r, escape_slope = 65)
plot(d, main = "Distance to escape terrain")
```

## Vector Ruggedness Measure - Local (VRML) [vrml()]

```r
library(ecologytools)
library(terra)
# create example DEM
r <- rast(nrows = 100, ncols = 100, xmin = 0, xmax = 100, ymin = 0, ymax = 100)
xy <- as.data.frame(crds(r))
z <- with(
  xy,
  800 +
    250 * exp(-((x - 30)^2 + (y - 35)^2) / 250) +
    180 * exp(-((x - 70)^2 + (y - 65)^2) / 180) -
    120 * exp(-((x - 55)^2 + (y - 45)^2) / 120)
)
values(r) <- z
# compute local ruggedness
rug <- vrml(r, s = 5)
# plot
plot(rug, main = "Vector Ruggedness of Local Relief")
```

## Days Since Peak IRG [days_since_peak_IRG()]

```r
library(ecologytools)
library(terra)
# create example time-series raster (5 layers)
r <- rast(nrows = 50, ncols = 50, nlyrs = 5)
values(r) <- runif(ncell(r) * nlyr(r))
# compute time since peak IRG
out <- days_since_peak_IRG(r)
# absolute distance from peak (optional)
out_abs <- days_since_peak_IRG(r, absolute_value = TRUE)
# plot one layer
plot(out, main = "Days Since Peak IRG")
```

## Aggregate Raster [aggregate_raster()]

```r
library(ecologytools)
library(terra)
# create example raster with 12 layers (e.g., daily data)
r <- rast(nrows = 50, ncols = 50, nlyrs = 12)
values(r) <- runif(ncell(r) * nlyr(r))
# aggregate into 3-layer periods (e.g., 3-day means)
out <- aggregate_raster(r, n = 3, fun = mean)
# plot first aggregated layer
plot(out, main = "Aggregated Raster")
```

## Force a Raster Into Memory [force_inmemory()]

```r
library(ecologytools)
library(terra)
# a raster that may reference an on-disk source
r <- rast(nrows = 50, ncols = 50, vals = runif(2500))
# read all values into memory so the object is safe to serialize
r <- force_inmemory(r)
inMemory(r)
# now safe to wrap + save (e.g., for a compute cluster)
# saveRDS(wrap(r), "raster.rds")
```

## Raster Summary [raster_summary()]

```r
library(ecologytools)
library(terra)
r <- rast(nrows = 10, ncols = 10, vals = runif(100))
# prints a formatted diagnostic summary (class, layers, extent, CRS, range, ...)
raster_summary(r, name = "example_raster")

# also accepts a PackedSpatRaster; it is unwrapped automatically
raster_summary(wrap(r), name = "packed_raster")
```

## Cluster GPS Locations into Search Areas [sheep_cluster()]

```r
library(ecologytools)
set.seed(1)
# synthetic GPS data: two individuals, projected coordinates, POSIXct times
gps_df <- data.frame(
  x = c(rnorm(30, 500000, 50), rnorm(30, 500500, 50)),
  y = c(rnorm(30, 4800000, 50), rnorm(30, 4800400, 50)),
  ID = rep(c("ewe_01", "ewe_02"), each = 30),
  DateTime = Sys.time() - runif(60, 0, 6 * 86400)
)

# DBSCAN-clustered convex-hull search areas from recent locations.
# NOTE: also writes last_locations.kml, movement_paths.kml, and
# search_areas.kml to the current working directory.
areas <- sheep_cluster(
  GPS = gps_df,
  no.days = 7,
  minimum.points = 10,
  epsilon = 100,
  crs = "EPSG:32612",
  filetype = "KML",
  basic.plot = TRUE
)
areas
```

# Forage & Diet Tools

## Modal Phenology by Period [calc_phenology_mode()]

```r
library(ecologytools)
set.seed(1)
dat <- data.frame(
  Date = as.POSIXct("2022-07-01", tz = "UTC") +
    sample(0:430, 200, replace = TRUE) * 86400,
  Phenology = sample(c("N/B", "FL", "FR", "M", NA), 200, replace = TRUE),
  Species = sample(c("Vaccinium membranaceum", "Chimaphila umbellata"), 200, replace = TRUE),
  Genus = sample(c("Vaccinium", "Chimaphila"), 200, replace = TRUE),
  Family = sample(c("Ericaceae", "Rosaceae"), 200, replace = TRUE),
  Functional.Group = sample(c("EVERGREEN SHRUB", "DECIDUOUS SHRUB"), 200, replace = TRUE),
  Growth.Form = "SHRUB",
  stringsAsFactors = FALSE
)
# modal phenology code per 14-day period, for each grouping level
res <- calc_phenology_mode(dat, length = 14)
res$Genus
res$All
```

## Plant Forage-Quality Summaries [plant_quality_summary()]

```r
library(ecologytools)
forage <- data.frame(
  Code             = c("VACMEM", "VACMEM", "CARGEY", "CARGEY"),
  Scientific.Name  = c("Vaccinium membranaceum", "Vaccinium membranaceum",
                       "Carex geyeri", "Carex geyeri"),
  Genus            = c("Vaccinium", "Vaccinium", "Carex", "Carex"),
  Family           = c("Ericaceae", "Ericaceae", "Cyperaceae", "Cyperaceae"),
  Functional.Group = c("DECIDUOUS SHRUB", "DECIDUOUS SHRUB", "GRAMINOID", "GRAMINOID"),
  Growth.Form      = c("SHRUB", "SHRUB", "GRAMINOID", "GRAMINOID"),
  DE  = c(12.1, 11.8, 9.5, 9.9),   # digestible energy
  DP  = c(8.8, 9.2, 5.4, 5.8),     # digestible protein
  DMD = c(0.62, 0.60, 0.55, 0.57)  # dry matter digestibility
)
# mean, sd, and non-NA n for DE/DP/DMD at each available level
summaries <- plant_quality_summary(forage)
summaries$species
summaries$genus
```

## Diet Proportions from RRA Data [calc_diet_prop()]

```r
library(ecologytools)
# sample-by-taxon relative read abundance (RRA); rows already normalized
rra <- data.frame(
  "Vaccinium membranaceum" = c(0.5, 0.2),
  "Carex geyeri"           = c(0.3, 0.5),
  "Pinaceae"               = c(0.2, 0.3),
  check.names = FALSE
)
lookup <- data.frame(
  Scientific.Name = c("Vaccinium membranaceum", "Carex geyeri"),
  Genus           = c("Vaccinium", "Carex"),
  Family          = c("Ericaceae", "Cyperaceae"),
  Order           = c("Ericales", "Poales")
)
props <- calc_diet_prop(rra, lookup)
props$species
props$genus
```

## Weighted Diet Quality [calc_diet_quality()]

`calc_diet_quality()` ties the diet pipeline together: it weights each taxon's
stage-specific forage quality by its RRA to produce per-sample DP and DE. It
consumes a phenology table from `calc_phenology_mode()` (`mode`), a stage-keyed
quality table (`qual`, keyed by level name plus `Phenology` and `Part`), and a
taxonomy crosswalk (`lookup`).

```r
library(ecologytools)

# `mode` comes from calc_phenology_mode() (one modal-code column per period)
mode <- calc_phenology_mode(dat, length = 14)

# `qual` is a list of stage-specific quality tibbles by level, e.g.
#   qual$species, qual$genus, ... each with columns:
#   <level name>, Phenology, Part, DP_mean, DE_mean
# `lookup` is a crosswalk with lowercase columns:
#   species, genus, family, order, functional_group, growth_form
# `rra` has the meta columns in `meta_cols` plus one numeric column per taxon,
#   each row summing to 1.

out <- calc_diet_quality(
  rra      = rra,
  qual     = qual,
  mode     = mode,
  lookup   = lookup,
  meta_cols = c("sample_id", "animal_id", "date", "year", "UTME", "UTMN")
)
out$quality   # per-sample DP, DE, and coverage
out$dropped   # audit of taxa excluded from each sample, with reasons
```

# Base Utilities

## Time an Expression [time_it()]

```r
library(ecologytools)
# prints start/end timestamps and elapsed seconds; returns the result invisibly
result <- time_it(Sys.sleep(1), label = "short nap")
```

## Session Package Info [package_info()]

```r
library(ecologytools)
# prints R version, platform, and versions of attached packages
package_info()
```
