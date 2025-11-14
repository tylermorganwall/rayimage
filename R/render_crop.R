#'@title Crop Image
#'
#'@description Crops an image (or matrix) either by specifying axis-aligned limits
#'(`width_limits`/`height_limits`) or by specifying a centered box size via `center`.
#'
#'@param image Default `NULL`. 3-layer RGB/4-layer RGBA array, `rayimg` class, or filename of an image.
#'@param width_limits Default `NULL`. Length-2 numeric `(xmin, xmax)` along the width (x/columns).
#' Values in `[0,1]` are treated as fractions of image width; otherwise they are pixel indices.
#' Mutually exclusive with `center`. If provided alone, `height_limits` defaults to full height.
#'@param height_limits Default `NULL`. Length-2 numeric `(ymin, ymax)` along the height (y/rows).
#' Values in `[0,1]` are treated as fractions of image height; otherwise they are pixel indices.
#' Mutually exclusive with `center`. If provided alone, `width_limits` defaults to full width.
#'@param center Default `NULL`. Length-2 numeric `(height, width)` for a center-crop.
#' Values in `(0,1]` are treated as fractions of `(image height, image width)`; otherwise pixels.
#' Mutually exclusive with `width_limits`/`height_limits`.
#'@param filename Default `NULL`. The filename of the image to be saved. If this is not given, the image will be plotted instead.
#'@param preview Default `FALSE`. Whether to plot the cropped image, or just to return the values.
#'@return A `rayimg` array with the cropped region.
#'@export
#'@examples
#'# Left half of the image
#'if(run_documentation()){
#'render_crop(dragon,
#'            width_limits = c(0, 0.50),
#'            height_limits = c(0, 1)) |>
#'  plot_image()
#'}
#'# Right half of the image
#'if(run_documentation()){
#'render_crop(dragon,
#'            width_limits = c(0.50, 1),
#'            height_limits = c(0, 1)) |>
#'  plot_image()
#'}
#'# Top middle of the image (25%–50% height, 25%–75% width)
#'if(run_documentation()){
#'render_crop(dragon,
#'            height_limits = c(0.25, 0.50),
#'            width_limits  = c(0.25, 0.75)) |>
#'  plot_image()
#'}
#'# Top middle, explicit pixel coords (for a 200x200 image)
#'if(run_documentation()){
#'render_crop(dragon,
#'            height_limits = c(50, 100),
#'            width_limits  = c(50, 150)) |>
#'  plot_image()
#'}
#'# Center-crop: 50% height and width of the image
#'if(run_documentation()){
#'render_crop(dragon, center = c(0.5, 0.5)) |>
#'  plot_image()
#'}
#'# Center-crop: fixed 50x100 pixels
#'if(run_documentation()){
#'render_crop(dragon, center = c(50, 100)) |>
#'  plot_image()
#'}
render_crop = function(
	image = NULL,
	width_limits = NULL,
	height_limits = NULL,
	center = NULL,
	filename = NULL,
	preview = FALSE
) {
	if (is.null(image)) {
		stop("`image` must be provided.", call. = FALSE)
	}

	use_limits = !is.null(width_limits) || !is.null(height_limits)
	use_center = !is.null(center)

	if (use_limits && use_center) {
		stop(
			"Provide either `width_limits`/`height_limits` OR `center`, not both.",
			call. = FALSE
		)
	}
	if (!use_limits && !use_center) {
		stop(
			"You must provide `width_limits`/`height_limits`, or `center`.",
			call. = FALSE
		)
	}

	if (!is.null(filename)) {
		if (tools::file_ext(filename) != "png") {
			filename = paste0(filename, ".png")
		}
	}

	# Normalize input and preserve metadata
	temp_image = ray_read_image(image)
	imagetype = attr(temp_image, "filetype")
	img_source_linear = attr(temp_image, "source_linear")
	colorspace = attr(temp_image, "colorspace")
	white_current = attr(temp_image, "white_current")

	h = nrow(temp_image)
	w = ncol(temp_image)

	to_px_range = function(lims, len) {
		if (length(lims) != 2 || !is.numeric(lims)) {
			stop("Limits must be length-2 numeric.", call. = FALSE)
		}
		a = lims[1]
		b = lims[2]
		if (all(a >= 0, a <= 1, b >= 0, b <= 1)) {
			start = floor(min(a, b) * len) + 1L
			end = ceiling(max(a, b) * len)
		} else {
			start = floor(min(a, b))
			end = ceiling(max(a, b))
		}
		start = max(1L, start)
		end = min(len, end)
		if (end < start) {
			stop(
				"Computed limits are empty after clamping; check your inputs.",
				call. = FALSE
			)
		}
		c(start, end)
	}

	if (use_limits) {
		if (is.null(width_limits)) {
			width_limits = c(0, 1)
		}
		if (is.null(height_limits)) {
			height_limits = c(0, 1)
		}

		x_rng = to_px_range(width_limits, w)
		y_rng = to_px_range(height_limits, h)

		x_start = x_rng[1]
		x_end = x_rng[2]
		y_start = y_rng[1]
		y_end = y_rng[2]
	} else {
		if (length(center) != 2 || !is.numeric(center)) {
			stop(
				"`center` must be a length-2 numeric vector (height, width).",
				call. = FALSE
			)
		}
		box_h = center[1]
		box_w = center[2]
		box_h = if (box_h > 0 && box_h <= 1) {
			round(max(1, box_h * h))
		} else {
			round(box_h)
		}
		box_w = if (box_w > 0 && box_w <= 1) {
			round(max(1, box_w * w))
		} else {
			round(box_w)
		}
		box_h = min(max(1L, box_h), h)
		box_w = min(max(1L, box_w), w)

		y_start = floor((h - box_h) / 2) + 1L
		y_end = y_start + box_h - 1L
		x_start = floor((w - box_w) / 2) + 1L
		x_end = x_start + box_w - 1L
	}

	if (imagetype != "matrix") {
		temp_image = temp_image[y_start:y_end, x_start:x_end, , drop = FALSE]
	} else {
		temp_image = temp_image[y_start:y_end, x_start:x_end, drop = FALSE]
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
