#' @title Render Convert Colorspace
#' @description Convert between two RGB spaces via XYZ, with optional white adaptation. Operates on **linear** RGB.
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
#' @examples
#' if (run_documentation()) {
#' # Read photo, convert to ACEScg with CAT (scene)
#' 	photo = ray_read_image(sunset_image, normalize = FALSE)
#' 	photo_aces = render_convert_colorspace(
#' 		photo,
#' 		to_mats = CS_ACESCG,
#' 		adapt_white = TRUE
#' 	)
#' 	tmp_txt = tempfile(fileext = ".png")
#' 	render_text_image(
#' 		"Sunset",
#' 		size = 60,
#' 		filename = tmp_txt,
#' 		color = "#c300ffff",
#' 		background_alpha = 0
#' 	)
#' 	# Read logo (display-referred), convert primaries only (no CAT)
#' 	logo = ray_read_image(tmp_txt, normalize = FALSE) # sRGB/D65
#' 	logo_aces = render_convert_colorspace(
#' 		logo,
#' 		to_mats = CS_ACESCG,
#' 		adapt_white = FALSE
#' 	)
#'
#' 	# Composite in ACEScg, then display (plot_image converts to sRGB/D65 + OETF)
#' 	# Here, we also turn overlay conversion in [render_image_overlay()] off,
#'  # to show what happens when you don't account for the colorspace difference.
#'  # By default [render_image_overlay()] will do this for you.
#' 	comp1 = render_image_overlay(
#' 		photo_aces,
#' 		logo_aces,
#' 		convert_overlay_colorspace = FALSE
#' 	)
#' 	comp2 = render_image_overlay(
#' 		photo_aces,
#' 		logo,
#' 		convert_overlay_colorspace = FALSE
#' 	)
#'   #Note the color differences, which arise from the mismatched colorspace on the right.
#' 	 plot_image_grid(list(comp1, comp2), dim = c(1, 2))
#' }
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
	og_src = ray_read_image(image, normalize = FALSE)
	d = dim(og_src)
	if (length(d) != 3L || d[3] < 3L) {
		return(handle_image_output(og_src, filename = filename, preview = preview))
	}
	src = ray_read_image(image, normalize = FALSE)

	if (is.atomic(from_mats) && length(from_mats) == 1L && is.na(from_mats)) {
		from_mats = attr(src, "colorspace")
	}
	stopifnot(is.list(from_mats), is.list(to_mats))

	if (is.atomic(from_white) && length(from_white) == 1L && is.na(from_white)) {
		from_white = attr(src, "white_current")
	}
	if (is.atomic(to_white) && length(to_white) == 1L && is.na(to_white)) {
		to_white = to_mats$white_xyz
	}

	src_wp = get_whitepoint_xyz(from_white)
	dst_wp = get_whitepoint_xyz(to_white)

	# Idempotence: same primaries, no CAT, and equal whites
	same_mats = isTRUE(all.equal(from_mats$rgb_to_xyz, to_mats$rgb_to_xyz)) &&
		isTRUE(all.equal(from_mats$xyz_to_rgb, to_mats$xyz_to_rgb))
	same_white = isTRUE(all.equal(src_wp$value, dst_wp$value))
	if (same_mats && (!adapt_white || same_white)) {
		return(handle_image_output(src, filename = filename, preview = preview))
	}

	# Assert linear data
	if (!isTRUE(attr(src, "source_linear"))) {
		warning(
			"render_convert_colorspace(): input is not linear; convert with render_gamma_linear(..., TRUE) first."
		)
	}

	# RGB(from) -> XYZ
	xyz = apply_color_matrix(src, from_mats$rgb_to_xyz)

	# Optional CAT in XYZ
	if (adapt_white && !isTRUE(all.equal(src_wp$value, dst_wp$value))) {
		CAT = compute_cat_bradford(src_wp$value, dst_wp$value)
		xyz = apply_color_matrix(xyz, CAT)
	}

	# XYZ -> RGB(to)
	out = apply_color_matrix(xyz, to_mats$xyz_to_rgb)
	if (d[3] == 4L) {
		out[,, 4] = src[,, 4]
	} # preserve alpha

	# Tag with the target space
	white_tag = if (adapt_white) dst_wp else src_wp
	to_mats$white_name = white_tag$white_name

	out = rayimg(
		out,
		filetype = attr(src, "filetype"),
		source_linear = TRUE,
		colorspace = to_mats,
		white_current = white_tag$value
	)

	handle_image_output(out, filename = filename, preview = preview)
}
