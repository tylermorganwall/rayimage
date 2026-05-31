rgb_to_hex = function(x) {
  if (is.null(dim(x))) {
    x = matrix(x, ncol = length(x), byrow = TRUE)
  }
  if (!(ncol(x) %in% c(3, 4))) {
    stop("rgb_to_hex(): expected 3 or 4 columns (R,G,B[,A]).")
  }
  x = pmin(pmax(x, 0), 1)
  ints = round(x * 255)
  paste0(
    "#",
    apply(ints, 1, function(row) {
      paste(sprintf("%02X", row), collapse = "")
    })
  )
}

#' Convert Color
#'
#' @param color The color to convert. Can be either an R color string, or a
#' numeric scalar, RGB vector, RGBA vector, or RGB/RGBA matrix with values
#' between `0` and `1`.
#' @param as_hex Default `FALSE`. If `TRUE`, return hex colors.
#' @param single Default `FALSE`. If `TRUE`, require exactly one color.
#'
#' @return RGB or RGBA color vector, or a color matrix for multiple colors.
#' @keywords internal
#'
#' @examples
#' #none
convert_color = function(color, as_hex = FALSE, single = FALSE) {
  if (is.null(color) || length(color) == 0) {
    stop("invalid color")
  }
  if (is.character(color)) {
    color = t(grDevices::col2rgb(color, alpha = TRUE)) / 255
  } else {
    if (!is.numeric(color)) {
      stop("invalid color")
    }
    color_dim = dim(color)
    if (is.null(color_dim)) {
      if (length(color) == 1) {
        color = rep(color, 3)
      }
      if (
        !(length(color) %in%
          c(3, 4) ||
          length(color) %% 3 == 0 ||
          length(color) %% 4 == 0)
      ) {
        stop("invalid color")
      }
      color_channels = if (length(color) %% 3 == 0) 3 else 4
      color = matrix(
        color,
        ncol = color_channels,
        byrow = TRUE
      )
    } else {
      if (length(color_dim) != 2 || !(color_dim[2] %in% c(3, 4))) {
        stop("invalid color")
      }
    }
  }
  if (!all(is.finite(color))) {
    stop("invalid color")
  }
  if (!all(color >= 0 & color <= 1)) {
    stop("invalid color")
  }
  if (isTRUE(single) && nrow(color) != 1L) {
    stop("invalid color")
  }
  if (as_hex) {
    return(rgb_to_hex(color))
  }
  # return vector for single row, matrix otherwise
  if (nrow(color) == 1L) as.numeric(color[1, ]) else color
}
