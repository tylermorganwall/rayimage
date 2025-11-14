#'@title Clamp Image
#'
#'@description Clamps an image to a user-specified range, ignoring the alpha channel.
#'
#'@param image 3-layer RGB/4-layer RGBA array, `rayimg` class, or filename of an image.
#'@param min_value Default `0`. Minimum value to clamp the RGB channels in the image to.
#'@param max_value Default `1`. Maximum value to clamp the RGB channels in the image to.
#'@param filename Default `NA`. Filename
#'@param preview Default `FALSE`. If `TRUE`, it will display the image in addition
#'to returning it.
#'@param ... Arguments to pass to either `jpeg::readJPEG`, `png::readPNG`, or `tiff::readTIFF`.
#'
#'@return A `rayimg` RGBA array.
#'@import grDevices
#'@export
#'@examples
#'if(run_documentation()){
#'#The image of the unchanged image
#'range(dragon)
#'}
#'if(run_documentation()){
#'#Clamp the maximum and minimum values to one and zero
#'render_clamp(dragon) |>
#'  range()
#'}
render_clamp = function(
	image,
	min_value = 0,
	max_value = 1,
	filename = NA,
	preview = FALSE,
	...
) {
	image = ray_read_image(image) #Always output RGBA array
	imagetype = attr(image, "filetype")
	img_source_linear = attr(image, "source_linear")
	colorspace = attr(image, "colorspace")
	white_current = attr(image, "white_current")

	d = dim(image)
	alpha_idx = NA
	if (length(d) == 3) {
		if (d[3] == 4) {
			alpha_idx = 4
		}
		if (d[3] == 2) {
			alpha_idx = 2
		}
	}
	array_image = unclass(image)
	if (length(d) == 2) {
		array_image[image < min_value] = min_value
		array_image[image > max_value] = max_value
		array_image = array(array_image, dim = d)
	} else {
		if (is.na(alpha_idx)) {
			num_chans = 1:3
		} else {
			num_chans = seq_len(alpha_idx - 1)
		}
		array_image = unclass(image)
		for (chan in num_chans) {
			tmp_chan = array_image[,, chan]
			mat_d = dim(tmp_chan)
			tmp_chan[array_image[,, chan] < min_value] = min_value
			tmp_chan[array_image[,, chan] > max_value] = max_value
			array_image[,, chan] = array(tmp_chan, dim = mat_d)
		}
	}
	new_image = ray_read_image(
		array_image,
		filetype = imagetype,
		source_linear = img_source_linear,
		assume_colorspace = colorspace,
		assume_white = white_current
	)
	handle_image_output(new_image, filename = filename, preview = preview)
}
