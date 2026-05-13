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
