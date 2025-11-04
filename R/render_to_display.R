#' Render To Display
#' @description Convert an image from its working space to a display space, and optionally apply sRGB OETF for viewing.
#' @param image A `rayimg`, array, or filename.
#' @param display Default `CS_SRGB`. Target display RGB space.
#' @param encode Default `TRUE`. If `TRUE`, apply sRGB OETF (only valid when `display` is sRGB-like).
#' @param adapt_white Default `TRUE`. Whether to perform CAT from working white to display white.
#' @return A `rayimg` when `encode=FALSE`, or a `rayimg` with sRGB-encoded RGB when `encode=TRUE`.
#' @export
render_to_display = function(
	image,
	display = CS_SRGB,
	encode = TRUE,
	adapt_white = TRUE
) {
	img = ray_read_image(image, normalize = FALSE)
	# Ensure linear before color conversion
	if (!isTRUE(attr(img, "source_linear"))) {
		img = render_gamma_linear(img, srgb_to_linear = TRUE)
	}
	# Move primaries (and optionally white) to display
	moved = render_convert_colorspace(
		img,
		from_mats = NA,
		to_mats = display,
		adapt_white = adapt_white,
		from_white = NA,
		to_white = display$white_xyz
	)
	if (!encode) {
		return(moved)
	}
	# Only sRGB OETF is implemented here
	if (!identical(display$name, CS_SRGB$name)) {
		warning(
			"render_to_display(): encode=TRUE uses sRGB OETF; non-sRGB displays may require a different OETF/transfer."
		)
	}
	render_gamma_linear(moved, srgb_to_linear = FALSE)
}
