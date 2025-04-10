p1 <- data.frame(lon = -90.8, lat = 48.6) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84")

p1_profile_pts <- profile_points(p1,
  profile.azimuth = 135, profile.length = units::set_units(10, "km"),
  crs = sf::st_crs("EPSG:26915")
)
p1_profile <- profile_line(p1_profile_pts)

berlin <- c(13.4, 52.517) # lon, lat
tokyo <- c(139.767, 35.7) # lon, lat

berlin_tokyo <- sf::st_sfc(
  sf::st_point(berlin),
  sf::st_point(tokyo),
  crs = "WGS84"
)


# Create a random raster
set.seed(123)
r <- terra::rast(ncol = 10, nrow = 10, xmin = -150, xmax = -80, ymin = 20, ymax = 60, crs = "WGS84")
terra::values(r) <- runif(terra::ncell(r))

# Create a random profile
profile <- data.frame(lon = c(-140, -90), lat = c(55, 25)) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84")
test_swath <- swath_profile(profile, r, k = 2, dist = 1)

test_swath_stats <- swath_stats(test_swath, profile.length = profile_length(profile_line(profile)))

# test output ------------------------------------------------------------------

test_that("Output of functions is as expected", {
  expect_equal(as.numeric(profile_length(p1_profile)), 10000)
  expect_equal(as.numeric(point_distance(berlin, tokyo)), 247.40371005925425152)
  expect_identical(line_ends(profile_line(berlin_tokyo)), berlin_tokyo)
})

# test warning and messages ----------------------------------------------------

test_that("Expect warning", {
  expect_warning(profile_points(p1, 135, profile.length = 2))
})

# test type --------------------------------------------------------------------

test_that("type of object returned is as expected", {
  expect_s3_class(profile_azimuth(p1_profile), "units")
  expect_s3_class(profile_length(p1_profile), "units")
  expect_s3_class(p1_profile_pts, c("sf", "sfc"))
  expect_s3_class(p1_profile, c("sf", "sfc"))
  expect_equal(class(test_swath), "list")
  expect_s3_class(test_swath$lines, c("sf", "sfc"))
  expect_vector(test_swath$swath[, 3], ptype = numeric(), size = 5)
  expect_s3_class(test_swath_stats, c("tbl", "data.frame"))
  expect_vector(test_swath_stats$elevation, ptype = numeric(), size = 16)
})
