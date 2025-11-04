#'@title Read Image
#'
#'@description Reads an image from a file/array/matrix. From files, supports `JPEG`, `PNG`, `TIFF`, and `EXR` images.
#'
#' @param image 3-layer RGB/4-layer RGBA array, `rayimg` class, or filename of an image.
#' @param convert_to_array Default `TRUE`. Whether to convert 2D B&W images/matrices to RGBA arrays.
#' @param preview Default `FALSE`. If `TRUE`, it will display the image in addition
#'to returning it.
#' @param source_linear Default `NA`, automatically determined based on the image type.
#' Whether the image source is linear data or sRGB.  `FALSE` for matrices, arrays, and `EXR` files,
#' true for all other formats (`jpeg`, `png`, and `tiff`).
#' @param normalize Default `FALSE`. If `TRUE`, convert to `normalize_to` space on read. Note that
#' rayimg inputs will keep their colorspace and ignore this option--use `render_convert_colorspace()`
#' to change an existing rayimg to a new colorspace.
#' @param normalize_to Default `CS_ACESCG`. Target colorspace when `normalize=TRUE`.
#' @param normalize_adapt_white Default `TRUE`. If `TRUE`, Bradford-adapt source white to target white.
#' @param assume_colorspace Default `NULL`. A colorspace descriptor (e.g., `CS_SRGB`,
#'   `CS_ACESCG`). If given, the loaded image will be *tagged* with this space
#'   rather than the default inferred by file type. No pixel conversion is performed
#'   unless `normalize=TRUE`.
#' @param assume_white Default `NULL`. Scene/display white for the loaded image.
#'   Either a named white (`"D60","D65","D50","D55","D75","E"`) or XYZ with Y=1.
#'   If `NULL`, uses `assume_colorspace$white_xyz`.
#'
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
	normalize = FALSE,
	normalize_to = CS_ACESCG,
	normalize_adapt_white = TRUE,
	assume_colorspace = NULL,
	assume_white = NULL,
	...
) {
	decode_if_needed = function(img, linear_flag) {
		if (isTRUE(linear_flag)) {
			return(list(image = img, source_linear = TRUE))
		}
		#Strip out NAs
		img[is.na(img)] = 0

		if (length(dim(img)) == 3) {
			if (dim(img)[3] >= 3) {
				img[,, 1:3] = to_linear(img[,, 1:3])
			} else {
				img = to_linear(img)
			}
		} else {
			img = to_linear(img)
		}
		list(image = img, source_linear = TRUE)
	}

	get_wp = function(white_point) {
		wp = get_whitepoint_xyz(white_point)
		if (is.null(wp)) {
			return(NULL)
		}
		wp$value
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
		source_linear = !(imagetype %in% c("png", "jpg", "tif"))
	}

	finalize_image = function(img, img_type, linear_flag, default_cs) {
		decoded = decode_if_needed(img, linear_flag)
		cs = if (!is.null(assume_colorspace)) {
			assume_colorspace
		} else {
			switch(
				img_type,
				png = CS_SRGB,
				jpg = CS_SRGB,
				tif = CS_SRGB,
				exr = CS_ACESCG,
				array = default_cs,
				matrix = default_cs,
				CS_ACESCG
			)
		}
		wc = get_wp(if (!is.null(assume_white)) assume_white else cs$white_xyz)
		ri = rayimg(
			process_image_preview(decoded$image),
			filetype = img_type,
			source_linear = decoded$source_linear,
			colorspace = cs,
			white_current = wc
		)
		if (normalize) {
			ri = render_convert_colorspace(
				ri,
				from_mats = NA,
				to_mats = normalize_to,
				adapt_white = normalize_adapt_white,
				from_white = NA,
				to_white = normalize_to$white_xyz
			)
		}
		ri
	}

	if (inherits(image, "rayimg")) {
		cs_assigned = if (!is.null(assume_colorspace)) {
			assume_colorspace
		} else {
			attr(image, "colorspace")
		}
		wc_assigned = if (!is.null(assume_white)) {
			get_wp(assume_white)
		} else {
			attr(image, "white_current")
		}
		ri = rayimg(
			process_image_preview(image),
			filetype = attr(image, "filetype"),
			source_linear = attr(image, "source_linear"),
			colorspace = cs_assigned,
			white_current = wc_assigned
		)
		return(ri) # no normalization on rayimg here
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
				colorspace = if (!is.null(assume_colorspace)) {
					assume_colorspace
				} else {
					CS_ACESCG
				},
				white_current = if (!is.null(assume_white)) {
					get_wp(assume_white)
				} else {
					CS_ACESCG$white_xyz
				}
			))
		} else {
			stop("The 'libopenexr' package is required for EXR support.")
		}
	} else {
		stop("Unsupported/unknown input type.")
	}
}
