## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE,warning=FALSE---------------------------------------
# Load packages required for this tutorial:
library(geoprofiler)
library(ggplot2)
library(units)
library(sf)
library(terra)
library(tidyterra)

theme_set(theme_bw())
options(ggplot2.continuous.colour = "viridis")
options(ggplot2.continuous.fill = "viridis")

## ----read, eval=FALSE, include=TRUE-------------------------------------------
# my_raster <- terra::rast("path/to/my/file.tif")

## ----load_data----------------------------------------------------------------
data("raster_example")
crs <- "EPSG:26915" # coordinate reference system for projection

my_raster <- terra::rast(raster_example, type = "xyz", crs = "WGS84") |>
  terra::project(crs)

elevation_map <- ggplot() +
  tidyterra::geom_spatraster(data = my_raster)

elevation_map

## ----azimuth, warning=FALSE, message=FALSE------------------------------------
my_profile <- data.frame(lon = -90.75, lat = 48.61) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84") |>
  profile_points(
    profile.azimuth = 135,
    profile.length = units::set_units(16, "km"),
    crs = sf::st_crs(crs)
  ) |>
  profile_line()

elevation_map +
  geom_sf(data = my_profile, lwd = 1)

## ----swath1, warning=FALSE, message=FALSE-------------------------------------
swath <- swath_profile(my_profile, raster = my_raster, k = 5, dist = 1000)

## ----swath_map, warning=FALSE, message=FALSE----------------------------------
elevation_map +
  geom_sf(data = swath$lines, lwd = .1)

## ----swath2, warning=FALSE, message=FALSE-------------------------------------
my_swath_profile <- swath_stats(swath, profile.length = profile_length(my_profile))

## ----plot, warning=FALSE, message=FALSE---------------------------------------
ggplot(my_swath_profile, aes(distance, elevation)) +
  geom_ribbon(aes(ymin = min, ymax = max), fill = "grey90") +
  geom_ribbon(aes(ymin = quantile25, ymax = quantile75), fill = "grey80") +
  # geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd), fill = "grey60") +
  geom_line(aes(y = median), color = "darkred", lwd = 1) +
  # geom_line(aes(y = mean), color = "dodgerblue", lwd = 1) +
  geom_line(lty = 2)

