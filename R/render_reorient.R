#'@title Reorient Image
#'
#'@description Reorients an image or matrix. Transformations are applied in this order: x, y, and transpose.
#'
#'@param image 3-layer RGB/4-layer RGBA array, `rayimg` class, or filename of an image.
#'@param flipx Default `FALSE`. Flip horizontally
#'@param flipy Default `FALSE`. Flip vertically.
#'@param transpose Default `FALSE`. Transpose image.
#'@param filename Default `NULL`. The filename of the image to be saved. If this is not given, the image will be plotted instead.
#'@param preview Default `FALSE`. Whether to plot the convolved image, or just to return the values.
#'@return 3-layer RGB reoriented array or matrix.
#'@export
#'@examples
#'if(run_documentation()){
#'#Original orientation
#'plot_image(dragon)
#'}
#'if(run_documentation()){
#'#Flip the dragon image horizontally
#'dragon |>
#'  render_reorient(flipx = TRUE) |>
#'  plot_image()
#'}
#'if(run_documentation()){
#'#Flip the dragon image vertically
#'dragon |>
#'  render_reorient(flipy = TRUE) |>
#'  plot_image()
#'}
#'if(run_documentation()){
#'#Transpose the dragon image
#'dragon |>
#'  render_reorient(transpose = TRUE) |>
#'  plot_image()
#'}
render_reorient = function(
	image,
	flipx = FALSE,
	flipy = FALSE,
	transpose = FALSE,
	filename = NULL,
	preview = FALSE
) {
	#Check if file or image before below:
	temp_image = ray_read_image(image, convert_to_array = FALSE)
	imagetype = attr(temp_image, "filetype")
	img_source_linear = attr(temp_image, "source_linear")
	colorspace = attr(temp_image, "colorspace")
	white_current = attr(temp_image, "white_current")

	if (flipx) {
		temp_image = fliplr(temp_image)
	}
	if (flipy) {
		temp_image = flipud(temp_image)
	}
	if (transpose) {
		if (imagetype == "matrix") {
			temp_image = t(temp_image)
		} else {
			temp_image = aperm(temp_image, c(2, 1, 3))
		}
	}
	temp_image = ray_read_image(
		temp_image,
		filetype = imagetype,
		source_linear = img_source_linear,
		assume_colorspace = colorspace,
		assume_white = white_current
	)

	handle_image_output(temp_image, filename = filename, preview = preview)
}
