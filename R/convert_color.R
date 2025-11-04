rgb_to_hex = function(x) {
	if (is.null(dim(x))) {
		x = matrix(x, ncol = 3, byrow = TRUE)
	}
	if (ncol(x) != 3) {
		stop("rgb_to_hex(): expected 3 columns (R,G,B).")
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
#' @param color The color to convert. Can be either a hexadecimal code, or a numeric rgb
#' vector listing three intensities between `0` and `1`.
#'
#' @return Color vector
#' @keywords internal
#'
#' @examples
#' #none
convert_color = function(color, as_hex = FALSE) {
	if (inherits(color, "character")) {
		color = t(grDevices::col2rgb(color)) / 255
	} else if (is.null(dim(color))) {
		color = matrix(color, ncol = 3, byrow = TRUE)
	}
	if (!all(is.finite(color))) {
		stop("invalid color")
	}
	if (!all(color >= 0 & color <= 1)) {
		stop("invalid color")
	}
	if (as_hex) {
		return(rgb_to_hex(color))
	}
	# return vector for single row, matrix otherwise
	if (nrow(color) == 1L) as.numeric(color[1, ]) else color
}
