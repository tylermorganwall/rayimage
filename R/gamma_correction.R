#' sRGB to Linear
#'
#' @param x Values
#'
#' @returns Transformed values
#' @keywords internal
to_linear = function(x) {
  out = x
  low = x <= 0.04045
  out[low] = x[low] / 12.92
  out[!low] = ((x[!low] + 0.055) / 1.055)^2.4
  out
}

#' Linear to sRGB
#'
#' @param x Values
#'
#' @returns Transformed values
#' @keywords internal
to_srgb = function(x) {
  x = pmin(pmax(x, 0), 1)
  out = x
  low = x <= 0.0031308
  out[low] = 12.92 * x[low]
  out[!low] = 1.055 * x[!low]^(1 / 2.4) - 0.055
  out
}
