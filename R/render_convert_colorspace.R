#' @title Render Convert Colorspace
#' @description Convert between two RGB spaces via XYZ, with optional white adaptation.
#'
#' @param image 3-layer RGB/4-layer RGBA array, `rayimg`, or filename.
#' @param from_mats Default `NA`. Source space object; when `NA`, pulled from `attr(src,"colorspace")`.
#' @param to_mats   Default `CS_ACESCG`. Target space object.
#' @param adapt_white Default `TRUE`. Apply Bradford CAT from `from_white` to `to_white`.
#' @param from_white Default `NA`. Source white (name or XYZ, Y=1). When `NA`, uses `attr(src,"white_current")`.
#' @param to_white   Default `NA`. Target white (name or XYZ, Y=1). When `NA`, uses `to_mats$white_xyz`.
#' @param filename Default `NULL`. Output path.
#' @param preview  Default `FALSE`. If `TRUE`, display the image.
#' @return A `rayimg` RGBA array tagged with the **target** space.
#' @export
render_convert_colorspace = function(
	image,
	from_mats = NA,
	to_mats = CS_ACESCG,
	adapt_white = TRUE,
	from_white = NA,
	to_white = NA,
	filename = NULL,
	preview = FALSE
) {
	#Avoid infinite recursion since it's in ray_read_image, set normalize = FALSE
	og_src = ray_read_image(image, normalize = FALSE)
	d = dim(og_src)
	if (length(d) != 3L || d[3] < 3L) {
		return(handle_image_output(
			og_src,
			filename = filename,
			preview = preview
		))
	}
	src = ray_read_image(image, convert_to_array = TRUE, normalize = FALSE)

	auto_from = is.atomic(from_mats) &&
		length(from_mats) == 1L &&
		is.na(from_mats)
	if (auto_from) {
		from_mats = attr(src, "colorspace")
	}

	auto_fw = is.atomic(from_white) &&
		length(from_white) == 1L &&
		is.na(from_white)
	if (auto_fw) {
		from_white = attr(src, "white_current")
	}

	auto_tw = is.atomic(to_white) &&
		length(to_white) == 1L &&
		is.na(to_white)
	if (auto_tw) {
		to_white = to_mats$white_xyz
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
			wp = std[[toupper(white_point)]]
			if (is.null(wp)) {
				stop("Unknown white: ", white_point)
			}
			return(wp)
		}
		stopifnot(is.numeric(white_point), length(white_point) == 3)
		white_point / white_point[2]
	}

	# RGB(from) -> XYZ
	xyz = apply_color_matrix(src, from_mats$rgb_to_xyz)

	# Optional CAT in XYZ
	if (adapt_white) {
		CAT = compute_cat_bradford(
			get_whitepoint_xyz(from_white),
			get_whitepoint_xyz(to_white)
		)
		xyz = apply_color_matrix(xyz, CAT)
	}

	# XYZ -> RGB(to)
	out = apply_color_matrix(xyz, to_mats$xyz_to_rgb)
	if (d[3] == 4L) {
		#Don't need to worry about d[3] == 2 here, already exited
		out[,, 4] = src[,, 4]
	} # preserve alpha

	# Tag with the target space
	white_tag = if (adapt_white) {
		get_whitepoint_xyz(to_white)
	} else {
		get_whitepoint_xyz(from_white)
	}
	out = rayimg(
		out,
		filetype = attr(src, "filetype"),
		source_linear = TRUE,
		colorspace = to_mats,
		white_current = white_tag
	)
	handle_image_output(out, filename = filename, preview = preview)
}
