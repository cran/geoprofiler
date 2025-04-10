#' @importFrom sf st_cast st_coordinates
#' @importFrom stats coef lm
#' @importFrom units set_units
#' @keywords internal
interpolate_azimuth <- function(x) {
  pts <- st_cast(x, "POINT") |>
    st_coordinates() |>
    as.data.frame()

  azi <- lm(Y ~ X, data = pts) |>
    coef() |>
    atan() |>
    tectonicr::rad2deg()
  set_units(azi[2], "degree")
}

#' @importFrom sf st_cast st_coordinates
#' @keywords internal
npts <- function(x) {
  st_cast(x, "POINT") |>
    st_coordinates() |>
    nrow()
}


#' Profile End Point
#'
#' Create a end point along a profile line starting at a point with a defined
#' direction and length.
#'
#' @param start `sf` point object.
#' @param profile.azimuth numeric or `units` object. Direction of profile in
#' degrees if numeric.
#' @param profile.length numeric or `units` object.
#' @param crs Coordinate reference system. Should be parsed by [sf::st_crs()].
#' @param return.sf logical. Should the profile points be returned as a `sf`
#' object (`TRUE`, the default) object or as a data.frame.
#'
#' @note
#' Use metric values (meters, kilometers, etc) in case of a projected coordinate
#' reference frame, and degree
#' when geographical coordinate reference frame.
#'
#' @return class depends on `return.sf`.
#' @export
#' @importFrom sf st_as_sf st_coordinates st_crs
#'
#' @examples
#' p1 <- data.frame(lon = -90.8, lat = 48.6) |>
#'   sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84")
#' profile_points(p1,
#'   profile.azimuth = 135, profile.length = units::set_units(10, "km"),
#'   crs = sf::st_crs("EPSG:26915")
#' )
profile_points <- function(start, profile.azimuth, profile.length, crs = st_crs(start), return.sf = TRUE) {
  stopifnot(
    # inherits(start, 'sf'),
    # is.numeric(profile.azimuth),
    # inherits(profile.length, 'units'),
    is.logical(return.sf)
  )

  if (inherits(profile.azimuth, "units")) {
    profile.azimuth <- units::set_units(profile.azimuth, "degree") |>
      units::drop_units()
  }

  p1_trans <- st_transform(start, crs = crs) |>
    st_coordinates()
  a <- tectonicr:::tand(90 - profile.azimuth)
  b <- p1_trans[1, 2] + p1_trans[1, 1] / a

  if (sf::st_is_longlat(sf::st_transform(start, crs = crs))) {
    if (!inherits(profile.length, "units")) warning("Unit of profile.length not specified. Assuming unit is in degrees.")
    l <- units::set_units(profile.length, "degree") |>
      units::drop_units()
  } else {
    if (!inherits(profile.length, "units")) warning("Unit of profile.length not specified. Assuming unit is in meters.")
    l <- units::set_units(profile.length, "m") |>
      units::drop_units()
  }

  end <- c(
    X = p1_trans[1, 1] - tectonicr:::sind(90 - profile.azimuth) * l,
    Y = p1_trans[1, 2] - tectonicr:::cosd(90 - profile.azimuth) * l
  )
  profile <- rbind(pq = p1_trans, end) |> as.data.frame(row.names = c("start", "end"), col.names = c("X", "Y"))
  if (return.sf) {
    profile |> st_as_sf(coords = c("X", "Y"), crs = crs)
  } else {
    profile
  }
}

#' Combine Points to a Line
#'
#' @param x `sf` point object
#'
#' @returns `sf` line object
#' @export
#' @importFrom sf st_combine st_cast
#' @seealso [profile_points()]
#'
#' @examples
#' p1 <- data.frame(lon = -90.8, lat = 48.6) |>
#'   sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84")
#' profile_points(p1,
#'   profile.azimuth = 135, profile.length = 10000,
#'   crs = sf::st_crs("EPSG:26915")
#' ) |>
#'   profile_line()
profile_line <- function(x) {
  sf::st_combine(x) |>
    sf::st_cast("LINESTRING")
}


#' Azimuth Between Profile Points
#'
#' @param x `sf` point object. First point marks the start point.
#'
#' @importFrom sf st_coordinates
#' @importFrom tectonicr get_azimuth
#'
#' @returns Azimuth as `units` object
#'
#' @details
#' If only two points are given, the azimuth is calculated using triangulation
#' from the `tectonicr` package.
#' If more than two points are given, the azimuth is calculated using linear
#' interpolation in the coordinate reference frame given by `profile`.
#'
#' @export
#'
#' @seealso [profile_length()]
#'
#' @examples
#' p1 <- data.frame(lon = -90.8, lat = 48.6) |>
#'   sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84")
#'
#' profile_points(p1,
#'   profile.azimuth = 135, profile.length = 10000,
#'   crs = sf::st_crs("EPSG:26915")
#' ) |>
#'   profile_azimuth()
profile_azimuth <- function(x) {
  if (npts(x) > 2) {
    interpolate_azimuth(x)
  } else {
    profile_deg <- x |>
      sf::st_transform("WGS84") |>
      sf::st_coordinates()
    tectonicr::get_azimuth(profile_deg[1, 2], profile_deg[1, 1], profile_deg[2, 2], profile_deg[2, 1]) |>
      units::set_units("degree")
  }
}

#' Length of Profile
#'
#' @param x `sf` line object
#' @param ... (optional) passed on to [s2::s2_distance()]
#'
#' @return `units` object when coordinate system is set.
#' @importFrom sf st_length
#' @export
#'
#' @seealso [profile_azimuth()]
#'
#' @examples
#' p1 <- data.frame(lon = -90.8, lat = 48.6) |>
#'   sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84")
#' profile_points(p1,
#'   profile.azimuth = 135, profile.length = 10000,
#'   crs = sf::st_crs("EPSG:26915")
#' ) |>
#'   profile_line() |>
#'   profile_length()
profile_length <- function(x, ...) {
  sf::st_length(x, ...)
}


#' @title Distance Between Points
#'
#' @description This uses the **haversine** formula (by default) to calculate
#' the great-circle distance between two points, i.e., the shortest distance
#' over the earth's surface.
#'
#' @param a lon, lat coordinate of point 1
#' @param b lon, lat coordinate of point 2
#' @param ... parameters passed to [tectonicr::dist_greatcircle()]
#' @return units object giving the distance
#' @importFrom tectonicr dist_greatcircle
#' @export
#' @examples
#' berlin <- c(13.4, 52.517) # lon, lat
#' tokyo <- c(139.767, 35.7) # lon, lat
#' point_distance(berlin, tokyo)
point_distance <- function(a, b, ...) {
  stopifnot(is.numeric(a), is.numeric(b))

  a_rad <- (pi / 180 * a)
  b_rad <- (pi / 180 * b)

  if (is.null(dim(a_rad))) {
    a_rad <- t(a_rad)
  }
  if (is.null(dim(b_rad))) {
    b_rad <- t(b_rad)
  }

  tectonicr::dist_greatcircle(a_rad[, 1], a_rad[, 2], b_rad[, 1], b_rad[, 2], ...) |>
    units::set_units("km")
}

#' Extract End Points of a Line
#'
#' @param x `sf` line object
#'
#' @returns `sf` point object
#' @export
#' @importFrom sf st_cast
#'
#' @examples
#' p1 <- data.frame(lon = -90.8, lat = 48.6) |>
#'   sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84")
#' profile_points(p1,
#'   profile.azimuth = 135, profile.length = 10000,
#'   crs = sf::st_crs("EPSG:26915")
#' ) |>
#'   profile_line() |>
#'   line_ends()
line_ends <- function(x) {
  x_pts <- sf::st_cast(x, "POINT")
  start <- x_pts[1]
  end <- x_pts[length(x_pts)]
  c(start, end)
}

#' Draw a profile line or a point to retrieve coordinates
#'
#' Opens a plot window showing the the map with the data,
#' where the user can click profile coordinates.
#'
#' @param x `sf` object
#' @inheritParams graphics::locator
#' @param col color of line or point
#'
#' @returns `sf` object of the profile.
#'
#' @name draw
NULL

#' @rdname draw
#' @export
#' @importFrom graphics locator plot
get_coordinates <- function(x, n = 1, type = "o", col = "#B63679FF", ...) {
  stopifnot(is.integer(n))

  crds <- sf::st_coordinates(x)
  plot(crds[, "X"], crds[, "Y"], asp = 1, xlab = "x", ylab = "y", main = "Click for coordinates")
  # pts <- identify(crds[, "X"], crds[, "Y"], ..., col = 'red')
  #  x[pts, ]
  pts <- invisible(locator(n = n, type = type, col = col, ...))
  data.frame(x = pts$x, y = pts$y) |>
    sf::st_as_sf(coords = c("x", "y"), crs = sf::st_crs(x))
}

#' @rdname draw
#' @export
draw_profile <- function(x, n = 10, ...) {
  get_coordinates(x, n = n, ...) |>
    profile_line()
}
