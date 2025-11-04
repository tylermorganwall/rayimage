rgb_to_hex = function(x) {
	if (is.null(dim(x))) {
		x = matrix(x, ncol = length(x))
	}
	if (!(ncol(x) %in% c(3L, 4L))) {
		stop("rgb_to_hex(): expected 3 or 4 columns (R,G,B[,A]).")
	}
	x = pmin(pmax(x, 0), 1)
	ints = round(x * 255)
	apply(ints, 1, function(row) {
		paste0("#", paste(sprintf("%02X", row), collapse = ""))
	})
}


array_rgb_to_hex_colmajor = function(arr) {
	d = dim(arr)
	R = as.vector(arr[,, 1])
	G = as.vector(arr[,, 2])
	B = as.vector(arr[,, 3])
	if (d[3] >= 4L) {
		A = as.vector(arr[,, 4])
		return(rgb_to_hex(cbind(R, G, B, A)))
	}
	rgb_to_hex(cbind(R, G, B))
}

#' Render Gamma/Linear
#'
#' Convert sRGB-encoded color data to linear light (and vice versa).
#'
#' @param color A `rayimg`, hex color string (or vector), length-3 numeric RGB vector,
#'   or RGB/RGBA matrix.
#' @param srgb_to_linear Default `TRUE`. If `TRUE`, converts sRGB-encoded data to linear light.
#'   If `FALSE`, converts linear-light data to sRGB.
#' @param as_hex Default `FALSE`. When `srgb_to_linear = FALSE` and input is an RGB/RGBA
#'   matrix or array, returns hex: for matrices a length-`n` vector; for arrays a
#'   length-`prod(dim(x)[1:2])` vector in column-major order. Alpha (if present) is passed through.
#'
#' @return Same shape/type as input unless `as_hex = TRUE` on matrix/array with `srgb_to_linear = FALSE`,
#'   in which case a character vector of hex codes is returned.
#' @export
#' @examples
#' # Numeric RGB vector (sRGB -> linear -> back to sRGB)
#' srgb_vec = c(0.2, 0.4, 0.6)
#' linear_vec = render_gamma_linear(srgb_vec)
#' render_gamma_linear(linear_vec, srgb_to_linear = FALSE)
#'
#' # Hex input (single color)
#' linear_from_hex = render_gamma_linear("#336699")
#' render_gamma_linear(linear_from_hex, srgb_to_linear = FALSE, as_hex = TRUE)
#'
#' # Greyscale matrix (treated as image plane)
#' grey_image = matrix(c(0.1, 0.5, 0.9, 0.3), nrow = 2)
#' linear_grey = render_gamma_linear(grey_image)
#' render_gamma_linear(linear_grey, srgb_to_linear = FALSE, as_hex = TRUE)
#'
#' # Array with a single greyscale channel
#' grey_pixels = array(c(0.2, 0.7), dim = c(1, 2, 1))
#' linear_grey_arr = render_gamma_linear(grey_pixels)
#' render_gamma_linear(linear_grey_arr, srgb_to_linear = FALSE, as_hex = TRUE)
#'
#' # Array with greyscale + alpha
#' grey_alpha = array(c(0.3, 0.9, 0.6, 0.4), dim = c(1, 2, 2))
#' linear_grey_alpha = render_gamma_linear(grey_alpha)
#' render_gamma_linear(linear_grey_alpha, srgb_to_linear = FALSE, as_hex = TRUE)
#'
#' # RGB array (3 channels)
#' rgb_pixels = array(
#'   c(
#'     0.2, 0.4, 0.6,
#'     0.7, 0.1, 0.3
#'   ),
#'   dim = c(1, 2, 3)
#' )
#' linear_rgb = render_gamma_linear(rgb_pixels)
#' render_gamma_linear(linear_rgb, srgb_to_linear = FALSE, as_hex = TRUE)
#'
#' # RGBA array (alpha channel preserved)
#' rgba_pixels = array(
#'   c(
#'     0.2, 0.4, 0.6, 0.5,
#'     0.7, 0.1, 0.3, 0.8
#'   ),
#'   dim = c(1, 2, 4)
#' )
#' linear_rgba = render_gamma_linear(rgba_pixels)
#' render_gamma_linear(linear_rgba, srgb_to_linear = FALSE, as_hex = TRUE)
#'
#' # rayimg input retains metadata and supports hex conversion
#' linear_img = render_gamma_linear(ray_read_image(rgba_pixels))
#' render_gamma_linear(linear_img, srgb_to_linear = FALSE, as_hex = TRUE)
render_gamma_linear = function(color, srgb_to_linear = TRUE, as_hex = FALSE) {
	fxn = if (srgb_to_linear) to_linear else to_srgb

	if (inherits(color, "rayimg")) {
		out = color
		d = dim(out)
		if (is.null(d)) {
			out[] = fxn(out[])
		} else {
			arr = unclass(out)
			c = d[3]
			if (length(d) == 2L) {
				arr = fxn(arr) # greyscale matrix
			} else if (length(d) == 3L) {
				if (c >= 1L) {
					arr[,, 1] = fxn(arr[,, 1])
				} # grey or R
				if (c >= 3L) {
					# RGB
					arr[,, 2] = fxn(arr[,, 2])
					arr[,, 3] = fxn(arr[,, 3])
				}
				# alpha (2nd when c==2, 4th when c==4) untouched
			} else {
				stop("render_gamma_linear(): expected 2D matrix or 3D array in rayimg.")
			}
			out[] = arr
		}
		if (!srgb_to_linear && isTRUE(as_hex)) {
			d = dim(out)
			if (length(d) == 2L) {
				g = as.vector(unclass(out))
				return(rgb_to_hex(cbind(g, g, g)))
			}
			if (length(d) == 3L && d[3] >= 1L && d[3] <= 4L) {
				arr = unclass(out)
				if (d[3] == 1L) {
					g = as.vector(arr[,, 1])
					return(rgb_to_hex(cbind(g, g, g)))
				} else if (d[3] == 2L) {
					g = as.vector(arr[,, 1])
					a = as.vector(arr[,, 2])
					return(rgb_to_hex(cbind(g, g, g, a)))
				} else {
					return(array_rgb_to_hex_colmajor(arr))
				}
			}
			stop("render_gamma_linear(): unsupported rayimg shape for hex.")
		}
		attr(out, "source_linear") = srgb_to_linear
		return(out)
	}

	if (is.character(color)) {
		if (!length(color)) {
			stop("render_gamma_linear(): character input cannot be empty.")
		}
		if (!srgb_to_linear) {
			stop(
				"render_gamma_linear(): for hex input, only sRGB -> linear is supported."
			)
		}
		rgb = t(grDevices::col2rgb(color)) / 255
		lin = to_linear(rgb)
		return(if (nrow(lin) == 1L) as.numeric(lin[1, ]) else lin)
	}

	if (is.matrix(color)) {
		if (srgb_to_linear) {
			return(fxn(color)) # greyscale plane
		} else {
			srgb_grey = fxn(color)
			if (as_hex) {
				g = as.vector(srgb_grey)
				return(rgb_to_hex(cbind(g, g, g)))
			}
			return(srgb_grey)
		}
	}

	if (is.array(color)) {
		if (length(dim(color)) != 3L) {
			stop("render_gamma_linear(): array must be h*w*c.")
		}
		d = dim(color)
		c = d[3]
		out = color
		if (c >= 1L) {
			out[,, 1] = fxn(out[,, 1])
		}
		if (c >= 3L) {
			out[,, 2] = fxn(out[,, 2])
			out[,, 3] = fxn(out[,, 3])
		}
		if (!srgb_to_linear && as_hex) {
			if (c == 1L) {
				g = as.vector(out[,, 1])
				return(rgb_to_hex(cbind(g, g, g)))
			}
			if (c == 2L) {
				g = as.vector(out[,, 1])
				a = as.vector(out[,, 2])
				return(rgb_to_hex(cbind(g, g, g, a)))
			}
			return(array_rgb_to_hex_colmajor(out))
		}
		return(out)
	}

	if (is.numeric(color)) {
		if (length(color) != 3L) {
			stop("render_gamma_linear(): numeric input must be length-3 RGB.")
		}
		if (any(!is.finite(color)) || any(color < 0 | color > 1)) {
			stop("render_gamma_linear(): numeric RGB values must be between 0 and 1.")
		}
		out = fxn(color)
		if (!srgb_to_linear && as_hex) {
			return(rgb_to_hex(out))
		}
		return(out)
	}

	stop("render_gamma_linear(): unsupported input type.")
}
