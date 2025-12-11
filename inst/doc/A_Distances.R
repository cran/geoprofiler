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
library(dplyr)

theme_set(theme_bw())
options(ggplot2.continuous.colour = "viridis")
options(ggplot2.continuous.fill = "viridis")

## ----read, eval=FALSE, include=TRUE-------------------------------------------
# my_data <- sf::read_sf("path/to/my/file.shp")

## ----load_data----------------------------------------------------------------
data("quakes")
crs <- st_crs("EPSG:3460") # coordinate reference system for projection

# Convert to sf object and transform to  projected coordinates
quakes_sf <- st_as_sf(quakes, coords = c("long", "lat"), crs = "WGS84") |>
  st_transform(crs = crs)

quake_map <- ggplot() +
  geom_sf(aes(color = depth, size = mag), data = quakes_sf) +
  scale_x_continuous(breaks = seq(-360, 360, 5)) +
  scale_size_binned()

quake_map

## ----pts----------------------------------------------------------------------
profile_pts <- data.frame(lon = c(160, -170), lat = c(-15, -24)) |>
  st_as_sf(coords = c("lon", "lat"), crs = "WGS84") |> # convert to sf object
  st_transform(crs = crs) # transform to projected coordinates

## ----line---------------------------------------------------------------------
profile_l <- profile_line(profile_pts)

quake_map +
  geom_sf(data = profile_l, lwd = 1)

## ----parameters---------------------------------------------------------------
profile_azimuth(profile_l)
profile_length(profile_l)

## ----azimuth, warning=FALSE, message=FALSE------------------------------------
data.frame(lon = 160, lat = 15) |>
  st_as_sf(coords = c("lon", "lat"), crs = "WGS84") |>
  st_transform(crs = crs) |>
  profile_points(profile.azimuth = 112, profile.length = set_units(8000, km))

## ----draw, eval=FALSE, include=TRUE-------------------------------------------
# draw_profile(quakes_sf, n = 3)

## ----project------------------------------------------------------------------
quakes_profile <- profile_coords(quakes_sf, profile = profile_l) |>
  bind_cols(quakes_sf)

## ----profile_map--------------------------------------------------------------
quakes_profile |>
  # divide by 1000 for km:
  mutate(X = X / 1000, Y = Y / 1000) |>
  ggplot(aes(X, Y, color = depth, size = mag)) +
  geom_point() +
  geom_hline(yintercept = 0, lwd = 1) +
  scale_size_binned() +
  scale_x_continuous(breaks = seq(0, 3000, 250)) +
  scale_y_continuous(breaks = seq(-3000, 3000, 250)) +
  coord_fixed()

## ----shift--------------------------------------------------------------------
quakes_profile_shifted <- quakes_profile |>
  mutate(
    X = X / 1000, # in km
    Y = (Y / 1000) - 500 # in km and shifted by 500 km to the "North"
  )

ggplot(quakes_profile_shifted, aes(X, Y, color = depth, size = mag)) +
  geom_point() +
  geom_hline(yintercept = 0, lwd = 1) +
  scale_size_binned() +
  scale_x_continuous(breaks = seq(0, 3000, 250)) +
  scale_y_continuous(breaks = seq(-3000, 3000, 250)) +
  coord_fixed()

## ----filter-------------------------------------------------------------------
quakes_profile_filtered <- filter(
  quakes_profile_shifted,
  abs(Y) <= 750,
  X >= 1600
)

## ----profile_plot1------------------------------------------------------------
ggplot(quakes_profile_filtered, aes(X, depth, color = depth, size = mag)) +
  geom_point() +
  scale_size_binned("Richter Magnitude") +
  scale_y_reverse() +
  scale_x_continuous(guide = guide_axis(position = "top")) +
  labs(x = "Distance along profile (km)", y = "Depth (km)", color = "Depth (km)")

## ----profile_plot2------------------------------------------------------------
quakes_profile_shifted |>
  arrange(desc(abs(Y))) |> # sort data to have close datapoints in foreground
  ggplot(aes(X, depth, color = mag, size = abs(Y), alpha = abs(Y))) +
  geom_point() +
  scale_color_viridis_c("Richter Magnitude", option = "A") +
  scale_size_continuous("Distance from profile (km)", range = c(3, .1)) +
  scale_alpha_continuous("Distance from profile (km)", range = c(1, .1)) +
  scale_y_reverse() +
  scale_x_continuous(guide = guide_axis(position = "top")) +
  labs(x = "Distance along profile (km)", y = "Depth (km)") +
  coord_cartesian(expand = FALSE)

