#'@title Add Overlay
#'
#'@description Takes an RGB array/filename and adds an image overlay.
#'
#'@param image 3-layer RGB/4-layer RGBA array, `rayimg` class, or filename of an image.
#'@param image_overlay Default `NULL`. 3-layer RGB/4-layer RGBA array, `rayimg` class, or filename of an image.
#'This image will be resized to the dimension of the image if it does not match exactly.
#'@param rescale_original Default `FALSE`. If `TRUE`, function will resize the original image to match
#'the overlay.
#'@param alpha Default `NA`, using overlay's alpha channel. Otherwise, this sets the alpha transparency
#'by multiplying the existing alpha channel by this value (between 0 and 1).
#'@param convert_overlay_colorspace Default `TRUE`. Whether to convert the overlay's colorspace
#'to match the underlying image.
#'@param filename Default `NULL`. File to save the image to. If `NULL` and `preview = FALSE`,
#'returns an RGB array.
#'@param preview Default `FALSE`. If `TRUE`, it will display the image in addition
#'to returning it.

#'@return A `rayimg` RGBA array.
#'@import grDevices
#'@export
#'@examples
#'if(run_documentation()){
#'#Plot the dragon
#'plot_image(dragon)
#'}
#'if(run_documentation()){
#'#Add an overlay of a red semi-transparent circle:
#'circlemat = generate_2d_disk(min(dim(dragon)[1:2]))
#'circlemat = circlemat/max(circlemat)
#'
#'#Create RGBA image, with a transparency of 0.5
#'rgba_array = array(1, dim=c(nrow(circlemat),ncol(circlemat),4))
#'rgba_array[,,1] = circlemat
#'rgba_array[,,2] = 0
#'rgba_array[,,3] = 0
#'dragon_clipped = dragon
#'dragon_clipped[dragon_clipped > 1] = 1
#'render_image_overlay(dragon_clipped, image_overlay = rgba_array,
#'                  alpha=0.5, preview = TRUE)
#'}
#' if (run_documentation()) {
#' 	# Read photo, convert to ACEScg with CAT (scene)
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
#' 		color = "#c300ff",
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
#' 	) |>
#'    render_title(title_text = "#c300ff: Match",
#'                 title_color = "white", title_color = "#c300ff", title_bar_alpha=1)
#' 	comp2 = render_image_overlay(
#' 		photo_aces,
#' 		logo,
#' 		convert_overlay_colorspace = FALSE
#' 	) |>
#' 	 render_title(title_text = "#c300ff: Incorrect",
#'                title_color = "white", title_color = "#c300ff", title_bar_alpha=1)
#'
#' 	plot_image_grid(list(comp1, comp2), dim = c(1, 2))
#' }
render_image_overlay = function(
	image,
	image_overlay = NULL,
	rescale_original = FALSE,
	convert_overlay_colorspace = TRUE,
	alpha = NA,
	filename = NULL,
	preview = FALSE
) {
	if (is.null(image_overlay)) {
		stop("Need to pass in image to image_overlay argument.")
	}

	# Load as rayimg (RGBA + attrs)
	image = ray_read_image(image, convert_to_array = TRUE)
	image_colorspace = attr(image, "colorspace")
	image_whitepoint = attr(image, "white_current")
	image_overlay = ray_read_image(image_overlay, convert_to_array = TRUE)
	if (convert_overlay_colorspace) {
		image_overlay = render_convert_colorspace(
			image_overlay,
			to_mats = image_colorspace
		)
	}

	img_type = attr(image, "filetype")
	over_type = attr(image_overlay, "filetype")

	if (!rescale_original) {
		if (!all(dim(image)[1:2] == dim(image_overlay)[1:2])) {
			image_overlay = render_resized(image_overlay, dims = dim(image))
		}
	} else {
		if (!all(dim(image)[1:2] == dim(image_overlay)[1:2])) {
			image = render_resized(image, dims = dim(image_overlay))
		}
	}
	is_matrix_image = length(dim(image)) == 2
	is_matrix_image_overlay = length(dim(image_overlay)) == 2
	process_image = function(image_input) {
		is_matrix_image = length(dim(image_input)) == 2
		if (is_matrix_image) {
			image_tmp = array(image_input, dim = c(dim(image_input), 4))
			image_tmp[,, 4] = 1
		} else {
			if (dim(image_input)[3] == 2) {
				#Greyscale with alpha
				image_tmp = array(image_input[,, 1], dim = c(dim(image_input)[1:2], 4))
				image_tmp[,, 4] = image_input[,, 2]
			} else if (dim(image_input)[3] == 3) {
				#RGB
				image_tmp = array(1, dim = c(dim(image_input)[1:2], 4))
				image_tmp[,, 4] = 1
			} else {
				image_tmp = image_input
			}
		}
		return(image_tmp)
	}
	image = process_image(image)
	image_overlay = process_image(image_overlay)

	if (!is.na(alpha)) {
		stopifnot(alpha >= 0, alpha <= 1)
		image_overlay[,, 4] = image_overlay[,, 4] * alpha
	}

	Cb = image[,, 1:3]
	Ab = image[,, 4]
	Cf = image_overlay[,, 1:3]
	Af = image_overlay[,, 4]

	Af3 = array(Af, dim = c(dim(Af), 3))
	Ab3 = array(Ab, dim = c(dim(Ab), 3))

	Ao = pmin(pmax(Af + Ab * (1 - Af), 0), 1)
	num = Cf * Af3 + Cb * Ab3 * array(1 - Af, dim = c(dim(Af), 3))

	eps = 1e-8
	Co_lin = num / array(pmax(Ao, eps), dim = c(dim(Ao), 3))

	# ensure fully transparent pixels are black
	if (any(Ao <= eps)) {
		m = Ao <= eps
		Co_lin[,, 1][m] = 0
		Co_lin[,, 2][m] = 0
		Co_lin[,, 3][m] = 0
	}

	composite_image = array(1, dim = dim(image))
	composite_image[,, 4] = Ao
	composite_image[,, 1:3] = Co_lin

	composite_image = ray_read_image(
		composite_image,
		assume_colorspace = image_colorspace,
		assume_white = image_whitepoint,
		source_linear = TRUE
	)
	handle_image_output(composite_image, filename = filename, preview = preview)
}


#'@title Add Overlay (Deprecated)
#'
#'@description Takes an RGB array/filename and adds an image overlay.
#'
#'@param ... to pass to [render_image_overlay()].

#'@return A `rayimg` RGBA array.
#'@export
#'@examples
#'if(run_documentation()){
#'#Plot the dragon
#'plot_image(dragon)
#'}
#'if(run_documentation()){
#'#Add an overlay of a red semi-transparent circle:
#'circlemat = generate_2d_disk(min(dim(dragon)[1:2]))
#'circlemat = circlemat/max(circlemat)
#'
#'#Create RGBA image, with a transparency of 0.5
#'rgba_array = array(1, dim=c(nrow(circlemat),ncol(circlemat),4))
#'rgba_array[,,1] = circlemat
#'rgba_array[,,2] = 0
#'rgba_array[,,3] = 0
#'dragon_clipped = dragon
#'dragon_clipped[dragon_clipped > 1] = 1
#'add_image_overlay(dragon_clipped, image_overlay = rgba_array,
#'                  alpha=0.5, preview = TRUE)
#'}
add_image_overlay = function(...) {
	message("add_image_overlay() deprecated--use render_image_overlay() instead.")
	render_image_overlay(...)
}
