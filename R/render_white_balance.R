#' @title Render White Balance (Bradford CAT)
#' @description Chromatic-adapt an image from `reference_white` to `target_white` **in its working RGB space**.
#'
#' When `bake = FALSE` (the default), this should not result in any visible changes to the image when
#' plotted in rayimage via [plot_image()], as that function then corrects the white balance for
#' display. Set
#'
#' @param image Default `NULL`. 3-layer RGB/4-layer RGBA array, `rayimg`, or filename.
#' @param reference_white Default `NA`. Source white (XYZ Y=1 or named); when `NA`, uses `attr(img, "white_current")`.
#' @param target_white Default `"D60"`. Target white (XYZ Y=1 or named).
#' @param bake Default `FALSE`. `FALSE` = appearance-preserving
#'   (update tag to the new white). When `TRUE`, apply CAT but keep the original tag,
#'   producing a visible warmer/cooler result on display.
#' @param filename Default `NULL`. Output path.
#' @param preview Default `FALSE`. If `TRUE`, display the image.
#' @param bake Default `FALSE`. If `TRUE`, this will bake the color change into the image, and return the
#' old tag. This can be used for stylistic adjustments.
#' @export
#' @examples
#' # Not specifying a target white balance returns an unchanged image
#' render_white_balance(dragon, preview = TRUE)
#'
#' # Default white "D60", converting to D50 produces a warmer image
#' render_white_balance(dragon, preview = TRUE, target_white = "D50", bake = TRUE)
#'
#' # Default white "D60", converting to "D75" produces a cooler image
#' render_white_balance(dragon, preview = TRUE, target_white = "D75", bake = TRUE)
#'
#' # Set reference white to colder "D75", converting to "D50" produces a very warm image
#' render_white_balance(dragon, preview = TRUE,
#'                      reference_white= "D75", target_white = "D50", bake = TRUE)
#'
#' # With a real, non-rendered image:
#' render_white_balance(sunset_image, preview=TRUE)
#'
#' # Cooler
#' render_white_balance(sunset_image, preview=TRUE, target_white = "D75", bake = TRUE)
#'
#' # Warmer
#' render_white_balance(sunset_image, preview=TRUE, target_white = "D50", bake = TRUE)
render_white_balance = function(
	image,
	reference_white = NA,
	target_white = "D60",
	bake = FALSE,
	filename = NULL,
	preview = FALSE
) {
	src = ray_read_image(image, normalize = FALSE)
	cs = attr(src, "colorspace")

	if (
		is.atomic(reference_white) &&
			length(reference_white) == 1L &&
			is.na(reference_white)
	) {
		reference_white = attr(src, "white_current")
	}

	# If whites are identical, do nothing
	if (
		isTRUE(all.equal(
			get_whitepoint_xyz(reference_white)$value,
			get_whitepoint_xyz(target_white)$value
		))
	) {
		return(handle_image_output(src, filename = filename, preview = preview))
	}

	if (!bake) {
		# CAT + update tag (appearance-preserving)
		return(render_convert_colorspace(
			src,
			from_mats = cs,
			to_mats = cs,
			adapt_white = TRUE,
			from_white = reference_white,
			to_white = target_white,
			filename = filename,
			preview = preview
		))
	}

	# bake = TRUE, CAT but keep the original tag ----
	src_wp = get_whitepoint_xyz(reference_white)
	dst_wp = get_whitepoint_xyz(target_white)

	# Work in linear RGB
	if (!isTRUE(attr(src, "source_linear"))) {
		warning(
			"render_white_balance(bake = TRUE): input not linear; convert with render_gamma_linear(..., TRUE) first."
		)
	}

	# RGB(working) -> XYZ
	xyz = apply_color_matrix(src, cs$rgb_to_xyz)
	# CAT in XYZ
	CAT = compute_cat_bradford(src_wp$value, dst_wp$value)
	xyz = apply_color_matrix(xyz, CAT)
	# XYZ -> RGB(working)
	out = apply_color_matrix(xyz, cs$xyz_to_rgb)
	# preserve alpha
	d = dim(src)
	if (length(d) == 3L && d[3] == 4L) {
		out[,, 4] = src[,, 4]
	}

	out = rayimg(
		out,
		filetype = attr(src, "filetype"),
		source_linear = attr(src, "source_linear"),
		colorspace = cs, # primaries unchanged
		white_current = src_wp$value # Keep the original tag to bake in the new color
	)
	handle_image_output(out, filename = filename, preview = preview)
}
