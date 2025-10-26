#' @title Render Convert Colorspace
#'
#' @description Convert between two RGB spaces via XYZ, with optional white adaptation.
#'
#' @param image Default `NULL`. 3-layer RGB/4-layer RGBA array, `rayimg`, or filename.
#' @param from_mats Default `acescg_conversion_matrices`. Source RGB to XYZ.
#' @param to_mats Default `srgb_mats`. Target RGB to XYZ.
#' @param adapt_white Default `TRUE`. If `TRUE`, apply Bradford CAT from `from_white` to `to_white`.
#' @param from_white Default `"D60"`. Source white name or XYZ (Y=1).
#' @param to_white Default `"D65"`. Target white name or XYZ (Y=1).
#' @param filename Default `NULL`. Output path.
#' @param preview Default `FALSE`. If `TRUE`, display the image.
#' @return A `rayimg` RGBA array.
#' @export
render_convert_colorspace = function(
	image,
	from_mats = rayimage::acescg_conversion_matrices,
	to_mats = rayimage::srgb_conversion_matrices,
	adapt_white = TRUE,
	from_white = "D60",
	to_white = "D65",
	filename = NULL,
	preview = FALSE
) {
	src = ray_read_image(image, convert_to_array = TRUE)
	d = dim(src)
	if (length(d) != 3) {
		return(src)
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

	# RGB(from) -> XYZ
	xyz = apply_color_matrix(src, from_mats$rgb_to_xyz)

	if (adapt_white) {
		CAT = compute_cat_bradford(
			get_whitepoint_xyz(from_white),
			get_whitepoint_xyz(to_white)
		)
		xyz = apply_color_matrix(xyz, CAT)
	}

	# XYZ -> RGB(to)
	out = apply_color_matrix(xyz, to_mats$xyz_to_rgb)

	# preserve alpha
	if (dim(src)[3] == 4L) {
		out[,, 4] = src[,, 4]
	}

	out = ray_read_image(out)
	attr(out, "filetype") = attr(src, "filetype")
	attr(out, "source_linear") = attr(src, "source_linear")
	handle_image_output(out, filename = filename, preview = preview)
}
