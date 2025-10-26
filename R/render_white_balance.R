#' @title Render White Balance (Bradford CAT)
#' @description Chromatic-adapt an image from `reference_white` to `target_white` in the working space.
#'
#' @param image Default `NULL`. 3-layer RGB/4-layer RGBA array, `rayimg`, or filename.
#' @param reference_white Default `NA`, automatically pulled from image if available. Source white (Y=1 XYZ or named: "D60","D65","D50","E"...).
#' @param target_white Default `"D60"`. Target white (same format as `reference_white`).
#' @param filename Default `NULL`. Output path.
#' @param preview Default `FALSE`. If `TRUE`, display the image.
#' @return A `rayimg` RGBA array.
#' @export
#' @examples
#' # Not specifying a target white balance returns an unchanged image
#' render_white_balance(dragon, preview = TRUE)
#'
#' # Default white "D60", converting to D50 produces a warmer image
#' render_white_balance(dragon, preview = TRUE, target_white = "D50")
#'
#' # Default white "D60", converting to "D75" produces a cooler image
#' render_white_balance(dragon, preview = TRUE, target_white = "D75")
#'
#' # Set reference white to colder "D75", converting to "D50" produces a very warm image
#' render_white_balance(dragon, preview = TRUE,
#'                      reference_white= "D75", target_white = "D50")
#'
#' # With a real, non-rendered image:
#' render_white_balance(sunset_image, preview=TRUE)
#'
#' # Cooler
#' render_white_balance(sunset_image, preview=TRUE, target_white = "D75")
#'
#' # Warmer
#' render_white_balance(sunset_image, preview=TRUE, target_white = "D50")
render_white_balance = function(
	image,
	reference_white = NA,
	target_white = "D60",
	filename = NULL,
	preview = FALSE
) {
	stopifnot(target_white %in% c("D65", "D60", "D55", "D50", "D75", "E"))
	src = ray_read_image(image, convert_to_array = TRUE)
	colorspace_conversion_matrices = attr(image, "colorspace")
	if (is.na(reference_white)) {
		reference_white = colorspace_conversion_matrices$white_name
	}
	if (reference_white == target_white) {
		return(handle_image_output(src, filename = filename, preview = preview))
	}
	d = dim(src)
	is_arr = length(d) == 3
	if (!is_arr) {
		return(src) # Do nothing for matrices
	}

	get_whitepoint_xyz = function(white_point) {
		std = list(
			"D65" = c(0.95047, 1, 1.08883),
			"D60" = c(0.95264, 1, 1.00827),
			"D55" = c(0.95560, 1, 0.92149),
			"D50" = c(0.96422, 1, 0.82521),
			"D75" = c(0.94972, 1, 1.22638),
			"E" = c(1, 1, 1)
		)
		if (is.character(white_point) && length(white_point) == 1) {
			return(std[[toupper(white_point)]])
		}
		stopifnot(is.numeric(white_point), length(white_point) == 3)
		white_point / white_point[2]
	}

	src_wp = get_whitepoint_xyz(reference_white)
	dst_wp = get_whitepoint_xyz(target_white)

	# RGB(working) -> XYZ
	xyz = apply_color_matrix(src, colorspace_conversion_matrices$rgb_to_xyz)
	# CAT (Bradford) in XYZ
	CAT = compute_cat_bradford(src_wp, dst_wp)
	xyz = apply_color_matrix(xyz, CAT)
	# XYZ -> RGB(working)
	out = apply_color_matrix(xyz, colorspace_conversion_matrices$xyz_to_rgb)

	# preserve alpha
	if (dim(src)[3] == 4L) {
		out[,, 4] = src[,, 4]
	}

	out = ray_read_image(out)
	attr(out, "filetype") = attr(src, "filetype")
	attr(out, "source_linear") = attr(src, "source_linear")

	handle_image_output(out, filename = filename, preview = preview)
}

#' @title Bradford CAT matrix from source to target white
#' @param source_xyz Default `c(0.95264,1,1.00827)` (D60). Source XYZ white, Y=1.
#' @param target_xyz Default `c(0.95047,1,1.08883)` (D65). Target XYZ white, Y=1.
#' @param eps Default `1e-8`. Small epsilon to avoid division by tiny LMS.
#' @return 3x3 numeric CAT matrix to apply in XYZ.
#' @keywords internal
compute_cat_bradford = function(
	source_xyz = c(0.95264, 1.0, 1.00827),
	target_xyz = c(0.95047, 1.0, 1.08883),
	eps = 1e-8
) {
	# fmt: skip
	B = matrix(
		c(
			0.8951, 0.2664, -0.1614,
			-0.7502, 1.7135, 0.0367,
			0.0389, -0.0685, 1.0296
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

#' @title Apply a 3x3 color transform to an image
#' @param img 3/4-channel array (RGB/A). 2D (grey) or 2-channel (grey+alpha) are passed through.
#' @param M   3x3 matrix applied to RGB in linear.
#' @return Array with transformed RGB; alpha preserved; greyscale inputs unchanged.
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

	rgb_idx = 1:3
	flat = matrix(img[,, rgb_idx], ncol = 3)
	flat = flat %*% t(M)
	out = img
	out[,, rgb_idx] = array(flat, dim = c(d[1], d[2], 3))
	out
}
