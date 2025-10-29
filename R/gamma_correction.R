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

#' Render Linear
#'
#' Convert sRGB encoded color data to linear light.
#'
#' @param color A `rayimg`, hex color string (or vector of strings), length-3 numeric RGB vector, or an
#'   array/matrix with color data (RGB in the last dimension when applicable).
#'
#' @return Linear RGB values matching the input type: a `rayimg` when supplied
#'   a `rayimg`, or a numeric/vector/array when supplied a color value.
#' @export
#' @examples
#' render_linear("#808080")
#' render_linear(c(0.5, 0.3, 0.1))
#' if (run_documentation()) {
#'   render_linear(render_white_balance(dragon, preview = FALSE))
#' }
render_linear = function(color) {
	linearize_channels = function(arr) {
		d = dim(arr)
		if (is.null(d)) {
			return(to_linear(arr))
		}
		if (length(d) == 2L) {
			return(to_linear(arr))
		}
		if (length(d) != 3L) {
			stop("render_linear(): expected a 2D matrix or 3D array.")
		}
		channels = d[3]
		out = arr
		if (channels <= 2L) {
			out[,, 1] = to_linear(out[,, 1])
		} else {
			idx = 1:3
			out[,, idx] = to_linear(out[,, idx, drop = FALSE])
		}
		out
	}

	if (inherits(color, "rayimg")) {
		if (isTRUE(attr(color, "source_linear"))) {
			return(color)
		}
		out = color
		d = dim(out)
		if (!is.null(d)) {
			converted = linearize_channels(unclass(out))
			out[] = converted
		} else {
			out[] = to_linear(out[])
		}
		attr(out, "source_linear") = TRUE
		return(out)
	}

	if (is.character(color)) {
		if (!length(color)) {
			stop("render_linear(): character input cannot be empty.")
		}
		rgb = grDevices::col2rgb(color) / 255
		linear_rgb = to_linear(rgb)
		linear_rgb = t(linear_rgb)
		if (nrow(linear_rgb) == 1L) {
			return(as.numeric(linear_rgb))
		}
		colnames(linear_rgb) = c("red", "green", "blue")
		rownames(linear_rgb) = NULL
		return(linear_rgb)
	}

	if (is.matrix(color)) {
		return(to_linear(color))
	}

	if (is.array(color)) {
		return(linearize_channels(color))
	}

	if (is.numeric(color)) {
		if (length(color) != 3L) {
			stop("render_linear(): numeric input must be a length-3 RGB vector.")
		}
		if (any(!is.finite(color))) {
			stop("render_linear(): numeric input must be finite.")
		}
		if (any(color < 0 | color > 1)) {
			stop("render_linear(): numeric RGB values must be between 0 and 1.")
		}
		return(to_linear(color))
	}

	stop("render_linear(): unsupported input type.")
}
