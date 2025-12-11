#' Swath Elevation Profile Statistics
#'
#' Calculate swath-profile values perpendicular to a straight baseline.
#' The distance between samples and the number of samples can be
#' specified, see arguments `k` and `dist`. Values of the swath-profile are
#' extracted from a given raster file, see argument `raster`. CRS of raster
#' and points have to be the same.
#'
#' @param profile either a `sf` object or a matrix(ncol=2, nrow=2) with x and
#' y coordinates of beginning and end point of the baseline; each point in one
#' row
#' \describe{
#'   \item{column 1}{x coordinates (or longitudes)}
#'   \item{column 2}{y coordinates (latitudes)}
#' }
#' @param raster Raster file (`"SpatRaster"` object as loaded by [terra::rast()])
#' @param k integer. number of lines on each side of the baseline
#' @param dist numeric. distance between lines
#' @param crs character. coordinate reference system. Both the `raster` and the
#' `profile` are transformed into this CRS. Uses the CRS of `raster` by default.
#' @param method character. method for extraction of raw data, see
#' [terra::extract()]: default value: `"bilinear"`
#'
#' @returns list.
#' \describe{
#'  \item{`swath`}{matrix. Statistics of the raster measured along the lines}
#'  \item{`data`}{list of numeric vector containing the data extracted from the
#'  raster along each line}
#'  \item{`lines`}{swath lines as `"sf"` objects}
#'  }
#'
#' @importFrom stats median quantile sd
#' @importFrom terra project vect as.lines geom crs extend extract ext
#' @importFrom sf st_linestring st_sfc st_geometry_type st_transform st_as_sf
#'
#' @details The final width of the swath is: \eqn{2k \times  \text{dist}}.
#'
#' @source The algorithm is a modified version of "swathR"
#' by Vincent Haburaj (https://github.com/jjvhab/swathR).
#'
#' @export
#'
#' @seealso [swath_stats()]
#'
#' @examples
#' # Create a random raster
#' r <- terra::rast(ncol = 10, nrow = 10, xmin = -150, xmax = -80, ymin = 20, ymax = 60, crs = "WGS84")
#' terra::values(r) <- runif(terra::ncell(r))
#'
#' # Create a random profile
#' profile <- data.frame(lon = c(-140, -90), lat = c(55, 25)) |>
#'   sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84")
#' swath_profile(profile, r, k = 2, dist = 1)
swath_profile <- function(profile, raster, k = 1, dist,
                          crs = terra::crs(raster),
                          method = c("bilinear", "simple")) {
  method <- match.arg(method)

  #------------------------------------------------------------#
  # Input handling (faster and more robust)
  #------------------------------------------------------------#
  raster <- terra::project(raster, crs)

  # Convert profile to SF line if necessary
  if (inherits(profile, "sf") && all(sf::st_geometry_type(profile) == "LINESTRING")) {
    profile <- line_ends(profile) # your function
  } else if (is.matrix(profile)) {
    profile <- sf::st_linestring(profile) |> sf::st_sfc(crs = crs)
  }

  profile <- sf::st_transform(profile, crs) |> terra::vect()

  #------------------------------------------------------------#
  # Geometry — compute perpendicular unit vector (NO trig cases!)
  #------------------------------------------------------------#
  g <- terra::geom(profile)[, c("x", "y")] # extract only numeric coords
  p1 <- g[1, ]
  p2 <- g[2, ]

  baseline <- p2 - p1
  baseline <- baseline / sqrt(sum(baseline^2)) # unit vector

  # Perpendicular vector
  perp <- c(-baseline[2], baseline[1])

  # Offset distances for each swath line
  offsets <- (-k:k) * dist

  #------------------------------------------------------------#
  # Generate all lines at once (no loop needed)
  #------------------------------------------------------------#
  lines_pts <- lapply(offsets, function(o) {
    shift <- perp * o
    m <- rbind(p1 + shift, p2 + shift)
    terra::vect(m, crs = crs) |> terra::as.lines()
  })

  #------------------------------------------------------------#
  # Extend raster *only if necessary*
  #------------------------------------------------------------#
  line_ext <- terra::ext(terra::vect(lines_pts))
  r_ext <- terra::ext(raster)

  need_expand <- !(r_ext[1] <= line_ext[1] && # xmin
    r_ext[2] >= line_ext[2] && # xmax
    r_ext[3] <= line_ext[3] && # ymin
    r_ext[4] >= line_ext[4]) # ymax

  if (need_expand) {
    raster <- terra::extend(raster, line_ext)
  }


  #------------------------------------------------------------#
  # Extract raster values (fast + simple)
  #------------------------------------------------------------#
  vals <- sapply(lines_pts, \(ln) terra::extract(raster, ln, method = method, ID = FALSE))

  #------------------------------------------------------------#
  # Vectorized statistics (minimal overhead)
  #------------------------------------------------------------#
  q <- t(vapply(
    vals, \(v) stats::quantile(v, c(.25, .50, .75), na.rm = TRUE),
    numeric(3)
  ))

  swath <- cbind(
    distance      = offsets,
    mean          = sapply(vals, mean, na.rm = TRUE),
    median        = q[, 2],
    sd            = sapply(vals, stats::sd, na.rm = TRUE),
    min           = sapply(vals, min, na.rm = TRUE),
    max           = sapply(vals, max, na.rm = TRUE),
    quantile25    = q[, 1],
    quantile75    = q[, 3]
  )

  list(
    swath = swath,
    data  = vals,
    lines = sf::st_as_sf(terra::vect(lines_pts))
  )
}

# swath_profile_old <- function(profile, raster, k = 1, dist, crs = terra::crs(raster), method = c("bilinear", "simple")) {
#   method <- match.arg(method)
#   raster <- terra::project(raster, crs)
#
#   if (inherits(profile, "sf") & all(sf::st_geometry_type(profile) == "LINESTRING")) {
#     profile <- line_ends(profile)
#   }
#
#   # create SpatialPoints from coords:
#   if (!inherits(profile, "sf") & is.matrix(profile)) {
#     profile <- sf::st_point(profile) |> sf::st_set_crs(crs)
#   }
#   coords_mat <- sf::st_coordinates(profile)
#
#   spt <- sf::st_transform(profile, crs = sf::st_crs(crs)) |> terra::vect()
#
#   # get slope of baseline:
#   m <- (terra::ymin(spt[1]) - terra::ymin(spt[2])) / (terra::xmin(spt[1]) - terra::xmin(spt[2]))
#   # get slope of normal function:
#   m1 <- -(1 / m)
#   # get slope-angle from slope:
#   alpha <- atan(m)
#   alpha1 <- atan(m1)
#   # get deltax and deltay from Pythagoras:
#   if ((alpha * 180) / pi < 90 & (alpha * 180) / pi > 270) {
#     deltax <- cos(alpha1) * dist
#   } else {
#     deltax <- cos(alpha1) * dist * -1
#   }
#   if ((alpha * 180) / pi > 0 & (alpha * 180) / pi < 180) {
#     deltay <- sqrt(dist**2 - deltax**2)
#   } else {
#     deltay <- sqrt(dist**2 - deltax**2) * -1
#   }
#   # create empty matrix:
#   swath <- matrix(nrow = k * 2 + 1, ncol = 8)
#   colnames(swath) <- c(
#     "distance", "mean", "median",
#     "std.dev.", "min", "max", "quantile(25)", "quantile(75)"
#   )
#   # list for spatial lines:
#   allLines <- list()
#   # add baseline:
#   allLines[[k + 1]] <- terra::vect(profile) |> terra::as.lines()
#   # set distance for baseline:
#   swath[k + 1, 1] <- 0
#   # generate k lines parallel to baseline:
#   for (n in 1:k) {
#     # BELOW BASELINE:
#     # new points
#     cn <- matrix(nrow = 2, ncol = 2)
#     cn[1, ] <- cbind(coords_mat[1, 1] - (deltax * n), coords_mat[1, 2] - (deltay * n))
#     cn[2, ] <- cbind(coords_mat[2, 1] - (deltax * n), coords_mat[2, 2] - (deltay * n))
#     # line between points:
#     allLines[[k + 1 - n]] <- terra::vect(cn, crs = crs) |> terra::as.lines()
#
#     # distance value:
#     swath[k + 1 - n, 1] <- -1 * n * dist
#     # ABOVE BASELINE:
#     # new points
#     cn <- matrix(nrow = 2, ncol = 2)
#     cn[1, ] <- cbind(coords_mat[1, 1] + (deltax * n), coords_mat[1, 2] + (deltay * n))
#     cn[2, ] <- cbind(coords_mat[2, 1] + (deltax * n), coords_mat[2, 2] + (deltay * n))
#     # line between points:
#     allLines[[k + n + 1]] <- terra::vect(cn, crs = crs) |> terra::as.lines()
#     # distance value:
#     swath[k + n + 1, 1] <- n * dist
#   }
#
#   # Expand raster to make sure all lines fall into the extent of the raster
#   lines_extent <- terra::vect(allLines) |> terra::ext()
#   raster_expanded <- terra::extend(raster, lines_extent)
#
#   # get raw data:
#   raw.data <- sapply(allLines, FUN = function(x) {
#     terra::extract(raster_expanded, x, method = method, ID = FALSE)
#   })
#
#   # generalise data:
#   swath[, 2] <- sapply(raw.data, mean, na.rm = TRUE)
#   swath[, 3] <- sapply(raw.data, stats::median, na.rm = TRUE)
#   swath[, 4] <- sapply(raw.data, stats::sd, na.rm = TRUE)
#   swath[, 5] <- sapply(raw.data, min, na.rm = TRUE)
#   swath[, 6] <- sapply(raw.data, max, na.rm = TRUE)
#   swath[, 7] <- sapply(raw.data, function(x) {
#     stats::quantile(x, na.rm = TRUE)[2]
#   })
#   swath[, 8] <- sapply(raw.data, function(x) {
#     stats::quantile(x, na.rm = TRUE)[4]
#   })
#
#   allLines_combined <- terra::vect(allLines) |> sf::st_as_sf()
#
#   list(swath = swath, data = raw.data, lines = allLines_combined)
# }


#' Summary Statistics on Swath Elevation Profile
#'
#' Statistics of the elevation data across a swath profile.
#'
#' @param x list. The return object of [swath_profile()]
#' @param profile.length numeric or `units` object. If `NULL` the fractional
#' distance is returned, i.e. 0 at start and 1 at the end of the profile.
#' @return data.frame
#'
#' @export
#'
#' @seealso [swath_profile()]
#'
#' @examples
#' # Create a random raster
#' r <- terra::rast(ncol = 10, nrow = 10, xmin = -150, xmax = -80, ymin = 20, ymax = 60)
#' terra::values(r) <- runif(terra::ncell(r))
#'
#' # Create a random profile
#' profile <- data.frame(lon = c(-140, -90), lat = c(55, 25)) |>
#'   sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84")
#' swath <- swath_profile(profile, r, k = 5, dist = 10)
#'
#' swath_stats(swath, profile.length = profile_length(profile_line(profile)))
swath_stats <- function(x, profile.length = 1) {
  # extract and compute target vector for "center elevation"
  data_list <- x$data
  max_length <- max(lengths(data_list))

  # preallocate numeric matrix to avoid repeated memory growth
  n <- length(data_list)
  elevs <- matrix(NA_real_, nrow = max_length, ncol = n)

  # fill matrix column-by-column (faster & memory-efficient)
  for (i in seq_len(n)) {
    li <- length(data_list[[i]])
    elevs[seq_len(li), i] <- data_list[[i]]
  }

  # summary stats by row (fully vectorized via apply)
  mins <- apply(elevs, 1, min, na.rm = TRUE)
  maxs <- apply(elevs, 1, max, na.rm = TRUE)
  means <- rowMeans(elevs, na.rm = TRUE)
  sds <- apply(elevs, 1, sd, na.rm = TRUE)
  meds <- apply(elevs, 1, median, na.rm = TRUE)
  q25 <- apply(elevs, 1, quantile, probs = 0.25, na.rm = TRUE)
  q75 <- apply(elevs, 1, quantile, probs = 0.75, na.rm = TRUE)

  # distance scaling
  d <- seq(0, 1, length.out = max_length) * profile.length

  # center elevation = median column
  center_col <- ceiling(n / 2)
  center_elev <- elevs[, center_col]

  data.frame(
    distance     = d,
    elevation    = center_elev,
    min          = mins,
    quantile25   = q25,
    median       = meds,
    quantile75   = q75,
    max          = maxs,
    mean         = means,
    sd           = sds
  )
}

# swath_stats_old <- function(x, profile.length = NULL) {
#   min <- mean <- sd <- quantile25 <- quantile75 <- median <- max <- distance <- NULL
#
#   if (is.null(profile.length)) profile.length <- 1
#   center <- paste0("X", as.character(median(seq_along(x$data))))
#
#   max_length <- max(sapply(x$data, length))
#
#   padded_vectors <- lapply(x$data, function(x) {
#     length(x) <- max_length
#     return(x)
#   })
#
#   # Combine the padded vectors into a matrix
#   elevs <- do.call(cbind, padded_vectors)
#   colnames(elevs) <- as.character(seq_along(x$data))
#
#   data.frame(elevs) |>
#     rowwise() |>
#     mutate(
#       min = min(c_across(everything()), na.rm = TRUE),
#       quantile25 = stats::quantile(c_across(everything()), na.rm = TRUE)[2],
#       median = stats::median(c_across(everything()), na.rm = TRUE),
#       quantile75 = stats::quantile(c_across(everything()), na.rm = TRUE)[4],
#       max = max(c_across(everything()), na.rm = TRUE),
#       mean = mean(c_across(everything()), na.rm = TRUE),
#       sd = stats::sd(c_across(everything()), na.rm = TRUE)
#       # CI95_low = stats::t.test(c_across(everything()))$conf.int[1],
#       # CI95_upp = stats::t.test(c_across(everything()))$conf.int[2]
#     ) |>
#     ungroup() |>
#     mutate(
#       "distance" = ((seq_along(elevs[, 1]) - 1) / (length(elevs[, 1]) - 1)) * profile.length,
#     ) |>
#     rename("elevation" = matches(center)) |>
#     select(distance, everything(), -starts_with("X"))
# }

# swath_stats_matrixstats <- function(x, profile.length = 1) {
#   data_list <- x$data
#   max_length <- max(lengths(data_list))
#   n <- length(data_list)
#
#   elevs <- matrix(NA_real_, max_length, n)
#   for(i in seq_len(n)) {
#     li <- length(data_list[[i]])
#     elevs[seq_len(li), i] <- data_list[[i]]
#   }
#
#   data.frame(
#     distance   = seq(0, 1, length.out = max_length) * profile.length,
#     elevation  = elevs[, ceiling(n/2)],
#     min        = matrixStats::rowMins(elevs, na.rm=TRUE),
#     quantile25 = matrixStats::rowQuantiles(elevs, probs=0.25, na.rm=TRUE),
#     median     = matrixStats::rowMedians(elevs, na.rm=TRUE),
#     quantile75 = matrixStats::rowQuantiles(elevs, probs=0.75, na.rm=TRUE),
#     max        = matrixStats::rowMaxs(elevs, na.rm=TRUE),
#     mean       = rowMeans(elevs, na.rm=TRUE),
#     sd         = matrixStats::rowSds(elevs, na.rm=TRUE)
#   )
# }
