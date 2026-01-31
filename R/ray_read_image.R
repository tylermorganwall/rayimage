#'@title Read Image
#'
#'@description Reads an image from a file/array/matrix. From files, supports
#'`JPEG`, `PNG`, `TIFF`, `TGA`, `BMP`, `PSD`, `GIF`, `HDR`, `PIC`, `PNM`, `EXR`,
#'and `DNG` images.
#'
#' @param image 3-layer RGB/4-layer RGBA array, `rayimg` class, or filename of an image.
#' @param convert_to_array Default `FALSE`. Whether to convert 2D B&W images/matrices to RGBA arrays.
#' @param preview Default `FALSE`. If `TRUE`, it will display the image in addition
#'to returning it.
#' @param source_linear Default `NA`, automatically determined based on the image type.
#' Whether the image source is linear data or sRGB. `TRUE` for matrices, arrays, and
#' floating-point HDR formats (`EXR`, `HDR`), and `DNG`; `FALSE` for all other file-based formats
#' (e.g., `jpeg`, `png`, `tiff`, `tga`, `bmp`, `psd`, `gif`, `pic`, `pnm`).
#' @param normalize Default `FALSE`. If `TRUE`, convert to `normalize_to` space on read. Note that
#' rayimg inputs will keep their colorspace and ignore this option--use `render_convert_colorspace()`
#' to change an existing rayimg to a new colorspace.
#' @param dng_normalize Default `FALSE`. If `TRUE`, DNG data is normalized to 0..1,
#' using black/white levels; if `FALSE`, raw values are returned.
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
#'@param ... Arguments to pass to `jpeg::readJPEG`, `png::readPNG`, `tiff::readTIFF`,
#'or `libopenexr::read_exr()` for supported formats.
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
	dng_normalize = FALSE,
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

	imagetype = if (
		is.character(image) &&
			tolower(tools::file_ext(image)) == "dng"
	) {
		"dng"
	} else {
		get_file_type(image)
	}
	if (is.na(source_linear)) {
		# arrays, matrices, and floating-point HDR formats are treated as linear
		source_linear = imagetype %in% c("array", "matrix", "exr", "hdr", "dng")
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
				tga = CS_SRGB,
				bmp = CS_SRGB,
				psd = CS_SRGB,
				gif = CS_SRGB,
				pic = CS_SRGB,
				pnm = CS_SRGB,
				hdr = CS_ACESCG,
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
			return(finalize_image(
				image[,, 1],
				imagetype,
				source_linear,
				CS_ACESCG
			))
		}
		return(finalize_image(image, imagetype, source_linear, CS_ACESCG))
	} else if (imagetype == "dng") {
		out = read_dng_cpp(image, normalize = dng_normalize)
		pixels = out$pixels
		meta = out$meta
		if (
			!is.null(meta$orientation) &&
				!is.na(meta$orientation) &&
				meta$orientation != 1
		) {
			orientation = as.integer(meta$orientation)
			flipx = FALSE
			flipy = FALSE
			transpose = FALSE
			if (orientation == 2L) {
				flipx = TRUE
			} else if (orientation == 3L) {
				flipx = TRUE
				flipy = TRUE
			} else if (orientation == 4L) {
				flipy = TRUE
			} else if (orientation == 5L) {
				transpose = TRUE
			} else if (orientation == 6L) {
				flipy = TRUE
				transpose = TRUE
			} else if (orientation == 7L) {
				flipx = TRUE
				flipy = TRUE
				transpose = TRUE
			} else if (orientation == 8L) {
				flipx = TRUE
				transpose = TRUE
			}
			if (flipx) {
				pixels = fliplr(pixels)
			}
			if (flipy) {
				pixels = flipud(pixels)
			}
			if (transpose) {
				if (length(dim(pixels)) == 2) {
					pixels = t(pixels)
				} else {
					pixels = aperm(pixels, c(2, 1, 3))
				}
				if (!is.null(meta$width) && !is.null(meta$height)) {
					tmp_dim = meta$width
					meta$width = meta$height
					meta$height = tmp_dim
				}
			}
			if (!is.null(meta$cfa_pattern)) {
				cfa = meta$cfa_pattern
				if (flipx) {
					cfa = cfa[, 2:1, drop = FALSE]
				}
				if (flipy) {
					cfa = cfa[2:1, , drop = FALSE]
				}
				if (transpose) {
					cfa = t(cfa)
				}
				meta$cfa_pattern = cfa
			}
			meta$orientation = 1L
		}
		attr(pixels, "dng") = meta
		if (!is.na(source_linear) && !source_linear) {
			pixels = decode_if_needed(pixels, source_linear)$image
		}
		return(finalize_image(pixels, "dng", TRUE, CS_ACESCG))
	} else if (imagetype == "png") {
		image = png::readPNG(image, ...)
		return(finalize_image(image, imagetype, source_linear, CS_SRGB))
	} else if (imagetype == "tif") {
		image = tiff::readTIFF(image, ...)
		return(finalize_image(image, imagetype, source_linear, CS_SRGB))
	} else if (imagetype == "jpg") {
		image = jpeg::readJPEG(image, ...)
		return(finalize_image(image, imagetype, source_linear, CS_SRGB))
	} else if (imagetype == "hdr") {
		image = read_image_stb(image, desired_channels = 4L)
		return(finalize_image(image, imagetype, source_linear, CS_ACESCG))
	} else if (imagetype %in% c("tga", "bmp", "psd", "gif", "pic", "pnm")) {
		image = read_image_stb(image, desired_channels = 4L)
		return(finalize_image(image, imagetype, source_linear, CS_SRGB))
	} else if (imagetype == "exr") {
		if (length(find.package("libopenexr", quiet = TRUE)) > 0) {
			tmp = libopenexr::read_exr(image, ...)
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
