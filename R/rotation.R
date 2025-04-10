# Helper functions used from {structr} package

#' Helper function to format vectors into 3-column matrices
#' @keywords internal
#' @param x numeric 3-column matrix or 3-element vector
#' @return numeric 3-column matrix
vec2mat <- function(x) {
  if (is.null(dim(x))) {
    as.matrix(t(x))
  } else {
    as.matrix(x)
  }
}

#' Length of vectors
#' @keywords internal
#' @param x numeric 3-column matrix
#' @return integer
vlength <- function(x) {
  sqrt(x[, 1]^2 + x[, 2]^2 + x[, 3]^2) |> unname()
}

#' Vector normalisation
#' @keywords internal
#' @param x numeric 3-column matrix
#' @return numeric 3-column matrix
vnorm <- function(x) {
  x / vlength(x)
}

#' Vector cross-product
#' @keywords internal
#' @param x,y numeric 3-column matrices representing vectors
#' @return numeric 3-column matrix
vcross <- function(x, y) {
  cbind(
    x[, 2] * y[, 3] - x[, 3] * y[, 2],
    x[, 3] * y[, 1] - x[, 1] * y[, 3],
    x[, 1] * y[, 2] - x[, 2] * y[, 1]
  )
}

#' Vector rotation
#'
#' Vector rotation about a given axis and angle after Schaeben et al. (2024)
#'
#' @keywords internal
#' @param x,rotaxis numeric 3-column matrices representing vectors
#' @param rotangle numeric. Angle in radians
#'
#' @references Schaeben, H., Kroner, U., Stephan, T., 2024. "Mathematical
#' fundamentals of spherical kinematics of plate tectonics in terms of
#' quaternions". *Math. Methods Appl. Sci.* 47, 4469–4496. \doi{10.1002/mma.9823}
#'
#' @return numeric 3-column matrix
vrotate <- function(x, rotaxis, rotangle) {
  rotaxis <- vnorm(rotaxis)
  vax <- vcross(rotaxis, x)
  x + vax * sin(rotangle) + vcross(rotaxis, vax) * 2 * (sin(rotangle / 2))^2 # Helmut
}
