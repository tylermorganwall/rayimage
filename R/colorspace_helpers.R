#' Normalize/label a white point
#' @param white_point Default `NULL`. Named ("D65","D60","D55","D50","D75","E") or XYZ (length 3, any Y).
#' @returns list(value = XYZ with Y=1, white_name = name or "Custom")
#' @keywords internal
get_whitepoint_xyz = function(white_point = NULL) {
	std = list(
		"D65" = c(0.95047, 1, 1.08883),
		"D60" = c(0.95264, 1, 1.00827),
		"D55" = c(0.95560, 1, 0.92149),
		"D50" = c(0.96422, 1, 0.82521),
		"D75" = c(0.94972, 1, 1.22638),
		"E" = c(1, 1, 1)
	)
	if (is.null(white_point)) {
		return(NULL)
	}
	if (is.character(white_point) && length(white_point) == 1L) {
		wp = std[[toupper(white_point)]]
		if (is.null(wp)) {
			stop("Unknown white: ", white_point)
		}
		return(list(value = wp, white_name = white_point))
	}
	stopifnot(is.numeric(white_point), length(white_point) == 3L)
	Y = white_point[2]
	if (!is.finite(Y) || Y <= 0) {
		stop("White Y must be positive and finite.")
	}
	list(value = white_point / Y, white_name = "Custom")
}

#' Bradford CAT matrix from source to target white
#' @param source_xyz Default `c(0.95264,1,1.00827)`. Source XYZ white, Y=1.
#' @param target_xyz Default `c(0.95047,1,1.08883)`. Target XYZ white, Y=1.
#' @param eps Default `1e-8`. Small epsilon.
#' @return 3x3 numeric CAT matrix to apply in XYZ.
#' @keywords internal
compute_cat_bradford = function(
	source_xyz = c(0.95264, 1.0, 1.00827),
	target_xyz = c(0.95047, 1.0, 1.08883),
	eps = 1e-8
) {
	B = matrix(
		c(
			0.8951,
			0.2664,
			-0.1614,
			-0.7502,
			1.7135,
			0.0367,
			0.0389,
			-0.0685,
			1.0296
		),
		3,
		3,
		byrow = TRUE
	)
	src = B %*% source_xyz
	dst = B %*% target_xyz
	if (any(abs(src) < eps)) {
		stop("CAT: source LMS too close to zero")
	}
	S = diag(as.numeric(dst / src))
	solve(B) %*% S %*% B
}

#' Apply a 3x3 color transform to an image
#' @param img 3/4-channel array (RGB/A). 2D (grey) or 2-channel (grey+alpha) pass through.
#' @param M 3x3 matrix.
#' @return Array with transformed RGB; alpha preserved.
#' @keywords internal
apply_color_matrix = function(img, M = diag(3)) {
	stopifnot(is.array(img), is.matrix(M), all(dim(M) == c(3, 3)))
	d = dim(img)
	if (length(d) == 2L) {
		return(img)
	}
	ch = d[3]
	if (ch < 3L) {
		return(img)
	}
	flat = matrix(img[,, 1:3], ncol = 3)
	flat = flat %*% t(M)
	out = img
	out[,, 1:3] = array(flat, dim = c(d[1], d[2], 3))
	out
}

#' sRGB to Linear
#' @param x Values
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
#' @param x Values
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

#' Encode RGB/RGBA to hex (sRGB domain)
#' @param x Values. 3 or 4 columns (R,G,B,A) in 0 to 1.
#' @return Hex vector (#RRGGBB or #RRGGBBAA).
#' @keywords internal
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

#' Array hex vector in column-major order
#' @param arr Array with c>=3
#' @return Hex vector length h*w
#' @keywords internal
array_rgb_to_hex_colmajor = function(arr) {
	d = dim(arr)
	if (length(d) != 3L || d[3] < 3L) {
		stop("array_rgb_to_hex_colmajor(): need h*w*(3|4).")
	}
	R = as.vector(arr[,, 1])
	G = as.vector(arr[,, 2])
	B = as.vector(arr[,, 3])
	if (d[3] >= 4L) {
		A = as.vector(arr[,, 4])
		return(rgb_to_hex(cbind(R, G, B, A)))
	}
	rgb_to_hex(cbind(R, G, B))
}
