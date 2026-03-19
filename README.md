# Install with remotes package

```r
install.packages("remotes")
remotes::install_github("RyanMartinEcology/ecologytools")
library(ecologytools)
```

# Install with devtools package

```r
install.packages("devtools")
devtools::install_github("RyanMartinEcology/ecologytools")
library(ecologytools)
```

## Plot Utility Examples 

```r
library(ggplot2)
library(ecologytools)

#---------------------------------------
# 1. Bar chart
#---------------------------------------
df_bar <- data.frame(
  group = LETTERS[1:5],
  value = c(10, 15, 7, 20, 12)
)

ggplot(df_bar, aes(x = group, y = value, fill = group)) +
  geom_col() +
  theme_martin(fill = "discrete", legend.position = "none")


#---------------------------------------
# 2. Scatter plot with many colors
#---------------------------------------
set.seed(1)
df_point <- data.frame(
  x = rnorm(200),
  y = rnorm(200),
  group = sample(letters[1:10], 200, replace = TRUE)
)

ggplot(df_point, aes(x, y, color = group)) +
  geom_point(size = 2) +
  theme_martin(color = "discrete")


#---------------------------------------
# 3. Line plot with many groups
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
  group = rep(paste0("g", 1:6), each = 50)
)

ggplot(df_line, aes(x, y, color = group)) +
  geom_line(linewidth = 1) +
  theme_martin(color = "gradient_earth_discrete")


#---------------------------------------
# 4. Facet wrap example
#---------------------------------------
df_facet <- data.frame(
  x = rep(1:20, 4),
  y = rnorm(80),
  group = rep(letters[1:4], each = 20)
)

ggplot(df_facet, aes(x, y)) +
  geom_line(color = "#253494", linewidth = 1) +
  facet_wrap(~group) +
  theme_martin()
```
## Spatial utilities

### Distance to escape terrain

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
