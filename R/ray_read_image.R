#'@title Read Image
#'
#'@description Reads an image from a file/array/matrix. From files, supports `JPEG`, `PNG`, `TIFF`, and `EXR` images.
#'
#'@param image 3-layer RGB/4-layer RGBA array, `rayimg` class, or filename of an image.
#'@param convert_to_array Default `TRUE`. Whether to convert 2D B&W images/matrices to RGBA arrays.
#'@param preview Default `FALSE`. If `TRUE`, it will display the image in addition
#'to returning it.
#'@param source_linear Default `NA`, automatically determined based on the image type.
#' Whether the image source is linear data or sRGB.  `FALSE` for matrices, arrays, and `EXR` files,
#' true for all other formats (`jpeg`, `png`, and `tiff`).
#'@param ... Arguments to pass to either `jpeg::readJPEG`, `png::readPNG`, `tiff::readTIFF`, or `libopenexr::read_exr()`.
#'
#'@return A `rayimg` RGBA array.
#'@import grDevices
#'@export
#'@examples
#'if(run_documentation()){
#'#Write as a png
#'tmparr = tempfile(fileext=".png")
#'ray_read_image(dragon) |>
#'  ray_write_image(tmparr)
#'ray_read_image(tmparr) |>
#'  plot_image()
#'}
#'if(run_documentation()){
#'#Write as a JPEG (passing quality arguments via ...)
#'tmparr = tempfile(fileext=".jpg")
#'ray_read_image(dragon) |>
#'  ray_write_image(tmparr, quality = 0.2)
#'ray_read_image(tmparr) |>
#'  plot_image()
#'}
#'if(run_documentation()){
#'#Write as a tiff
#'tmparr = tempfile(fileext=".tiff")
#'ray_read_image(dragon) |>
#'  ray_write_image(tmparr)
#'ray_read_image(tmparr) |>
#'   plot_image()
#'}
#'
#'if(run_documentation()){
#'#Write as an exr
#'tmparr = tempfile(fileext=".exr")
#'ray_read_image(dragon) |>
#'  ray_write_image(tmparr)
#'ray_read_image(tmparr) |>
#'   plot_image()
#'}
ray_read_image = function(
	image,
	convert_to_array = FALSE,
	preview = FALSE,
	source_linear = NA,
	...
) {
	decode_if_needed = function(img, linear_flag) {
		if (isTRUE(linear_flag)) {
			return(list(image = img, source_linear = TRUE))
		}
		# Not linear → decode RGB via sRGB EOTF
		if (length(dim(img)) == 3) {
			channels = dim(img)[3]
			if (channels >= 3) {
				img[,, 1:3] = to_linear(img[,, 1:3])
			} else {
				img = to_linear(img)
			}
		} else {
			img = to_linear(img)
		}
		list(image = img, source_linear = TRUE)
	}

	process_image_preview = function(image2) {
		if (convert_to_array) {
			image_val = array(1, dim = c(dim(image2)[1:2], 4))
			if (length(dim(image2)) == 2) {
				image_val[,, 1:3] = image2
			} else if (length(dim(image2)) == 3) {
				if (dim(image2)[3] == 2) {
					image_val[,, 1:3] = image2[,, 1]
					image_val[,, 4] = image2[,, 2]
				} else if (dim(image2)[3] == 3) {
					image_val[,, 1:3] = image2[,, 1:3]
				} else if (dim(image2)[3] == 4) {
					image_val = image2
				}
			}
		} else {
			image_val = image2
		}
		if (preview) {
			plot_image(image_val)
		}
		image_val
	}

	imagetype = get_file_type(image)
	if (is.na(source_linear)) {
		# LDR files are non-linear; arrays/matrices/EXR are linear
		source_linear = !(imagetype %in% c("png", "jpg", "tif"))
	}

	finalize_image = function(img, img_type, linear_flag, assumed_cs) {
		decoded = decode_if_needed(img, linear_flag)
		# Choose default colorspace by source type
		cs = switch(
			img_type,
			png = CS_SRGB,
			jpg = CS_SRGB,
			tif = CS_SRGB,
			exr = CS_ACESCG,
			array = assumed_cs,
			matrix = assumed_cs,
			CS_ACESCG
		)
		rayimg(
			process_image_preview(decoded$image),
			filetype = img_type,
			source_linear = decoded$source_linear,
			colorspace = cs,
			white_current = cs$white_xyz
		)
	}

	if (inherits(image, "rayimg")) {
		# normalize to rayimg with current attrs
		return(rayimg(
			process_image_preview(image),
			filetype = attr(image, "filetype"),
			source_linear = attr(image, "source_linear"),
			colorspace = attr(image, "colorspace"),
			white_current = attr(image, "white_current")
		))
	}
	if (imagetype == "array") {
		return(finalize_image(image, imagetype, source_linear, CS_ACESCG))
	} else if (imagetype == "matrix") {
		if (length(dim(image)) == 3) {
			return(finalize_image(image[,, 1], imagetype, source_linear, CS_ACESCG))
		}
		return(finalize_image(image, imagetype, source_linear, CS_ACESCG))
	} else if (imagetype == "png") {
		image = png::readPNG(image, ...)
		return(finalize_image(image, imagetype, source_linear, CS_SRGB))
	} else if (imagetype == "tif") {
		image = tiff::readTIFF(image, ...)
		return(finalize_image(image, imagetype, source_linear, CS_SRGB))
	} else if (imagetype == "jpg") {
		image = jpeg::readJPEG(image, ...)
		return(finalize_image(image, imagetype, source_linear, CS_SRGB))
	} else if (imagetype == "exr") {
		if (length(find.package("libopenexr", quiet = TRUE)) > 0) {
			tmp = libopenexr::read_exr(image)
			image = array(1, dim = c(tmp$height, tmp$width, 4))
			image[,, 1] = tmp$r
			image[,, 2] = tmp$g
			image[,, 3] = tmp$b
			return(rayimg(
				process_image_preview(image),
				filetype = imagetype,
				source_linear = TRUE,
				colorspace = CS_ACESCG,
				white_current = CS_ACESCG$white_xyz
			))
		} else {
			stop("The 'libopenexr' package is required for EXR support.")
		}
	} else {
		stop("This error should never be reached--please report a bug.")
	}
}
