#' Profile Coordinates
#'
#' Project points on a cross section given by a starting point and the direction
#'
#' @param x `'sf'` object
#' @param profile `'sf'` object of the profile or the profile's starting point.
#' @param azimuth numeric. Direction (in degrees) emanating from starting point.
#' Is ignored when `profile` contains two points or is a `LINESTRING`.
#' @param drop.units logical. Whether the return should show the units or not.
#'
#' @returns `tibble` where `X` is the distance along the profile line.
#' `Y` is the distance across the profile line. (units of `X` and `Y` depend on
#' coordinate reference system).
#'
#' @importFrom dplyr across as_tibble mutate tibble everything
#' @importFrom sf st_as_sf st_cast st_coordinates st_crs st_geometry_type st_is_longlat st_transform
#' @importFrom tectonicr deg2rad
#' @importFrom units drop_units set_units
#'
#' @author Tobias Stephan
#'
#' @export
#'
#' @examples
#' data(locations_example)
#' p1 <- data.frame(lon = -90.8, lat = 48.6) |>
#'   sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84")
#' profile_crds <- profile_coords(locations_example, profile = p1, azimuth = 135)
#' head(profile_crds)
#'
#' # Plot the transformed coordinates
#' plot(profile_crds)
profile_coords <- function(x, profile, azimuth = NULL, drop.units = TRUE) {
  X <- Y <- numeric()
  x2 <- st_transform(x, crs = "WGS84") |>
    st_coordinates() |>
    as_tibble() |>
    mutate(
      lon_rad = deg2rad(X),
      lat_rad = deg2rad(Y),
      R = 1
    )

  vec <- matrix(nrow = length(x2$X), ncol = 3)
  vec[, 1] <- x2$R * cos(x2$lat_rad) * cos(x2$lon_rad)
  vec[, 2] <- x2$R * cos(x2$lat_rad) * sin(x2$lon_rad)
  vec[, 3] <- x2$R * sin(x2$lat_rad)

  if (all(st_geometry_type(profile) == "LINESTRING")) {
    profile <- profile |> st_cast("POINT")
  }

  p <- profile[1, ] |>
    st_transform(crs = "WGS84") |>
    st_coordinates() |>
    as_tibble() |>
    mutate(across(everything(), deg2rad))

  p1_coords <- profile[1, ] |>
    st_transform(crs = st_crs(x)) |>
    st_coordinates()

  rot <- matrix(nrow = 1, ncol = 3)
  p1r <- 1
  rot[, 1] <- p1r * cos(p$Y) * cos(p$X)
  rot[, 2] <- p1r * cos(p$Y) * sin(p$X)
  rot[, 3] <- p1r * sin(p$Y)

  if (nrow(st_coordinates(profile)) == 2 & all(st_geometry_type(profile) == "POINT")) {
    azimuth <- profile_azimuth(profile)
  }

  if (inherits(azimuth, "units")) {
    azimuth <- set_units(azimuth, "degree") |>
      drop_units()
  }

  n <- vrotate(vec, rotaxis = rot, rotangle = deg2rad(azimuth + 90 + 180))
  r <- sqrt(n[, 1]^2 + n[, 2]^2 + n[, 3]^2)
  lat2 <- asin(n[, 3] / r) # lat
  lon2 <- atan2(n[, 2], n[, 1])

  res <- tibble(
    X = lon2, Y = lat2
  ) |>
    mutate(across(everything(), tectonicr::rad2deg)) |>
    st_as_sf(coords = c("X", "Y"), crs = "WGS84", na.fail = FALSE, remove = FALSE) |>
    st_transform(crs = st_crs(x)) |>
    st_coordinates() |>
    as_tibble()

  xmin <- min(res$X, na.rm = TRUE)

  res2 <- mutate(res,
    X = X - xmin,
    Y = Y - p1_coords[1, 2]
  )
  if (st_is_longlat(x)) {
    res3 <- mutate(res2, X = ifelse(X > 180, 360 - X, X)) |>
      mutate(across(everything(), ~ set_units(.x, value = "degree")))
  } else {
    res3 <- mutate(res2, across(everything(), ~ set_units(.x, value = "m")))
  }
  if (drop.units) {
    mutate(res3, across(everything(), drop_units))
  } else {
    res3
  }
}
