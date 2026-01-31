#'@title Stack Images
#'
#'@description Stacks a list of images vertically or horizontally into a single image.
#'
#'@param input_list List of 3-layer RGB/4-layer RGBA array, `rayimg` class, or image filenames.
#'@param stack Default `"v"`. Stack direction: `"v"`/`"vertical"` or `"h"`/`"horizontal"`.
#'@param align Default `"center"`. Alignment on the non-stacking axis. Options:
#'`"start"`/`"left"`/`"top"`, `"center"`, `"end"`/`"right"`/`"bottom"`.
#'@param background Default `c(0,0,0,0)`. Background color for padding, as numeric
#'`c(r,g,b[,a])` in 0..1 or a color string.
#'@param convert_colorspace Default `TRUE`. Whether to convert all images to match
#'the colorspace of the first image in the list.
#'@param filename Default `NULL`. File to save the image to. If `NULL` and `preview = FALSE`,
#'returns an RGB array.
#'@param preview Default `FALSE`. If `TRUE`, it will display the image in addition
#'to returning it.
#'
#'@return A `rayimg` RGBA array.
#'@export
#'@examples
#'if(run_documentation()){
#'render_stack(list(dragon, 1 - dragon), stack = "h") |>
#'  plot_image()
#'}
#'if(run_documentation()){
#'render_stack(list(dragon, render_bw(dragon)), stack = "v") |>
#'  plot_image()
#'}
render_stack = function(
	input_list,
	stack = c("v", "h", "vertical", "horizontal"),
	align = c("center", "start", "end", "left", "right", "top", "bottom"),
	background = c(0, 0, 0, 0),
	convert_colorspace = TRUE,
	filename = NULL,
	preview = FALSE
) {
	if (!inherits(input_list, "list") || length(input_list) == 0) {
		stop("`input_list` must be a non-empty list of images.")
	}
	if (any(vapply(input_list, is.null, logical(1)))) {
		stop("`input_list` cannot contain NULL entries.")
	}

	if (missing(stack)) {
		stack = "v"
	} else {
		stack = tolower(stack)
	}
	stack = match.arg(stack, c("v", "h", "vertical", "horizontal"))
	is_vertical = stack %in% c("v", "vertical")
	if (missing(align)) {
		align = "center"
	} else {
		align = tolower(align)
	}
	align = match.arg(
		align,
		c("center", "start", "end", "left", "right", "top", "bottom")
	)
	align = switch(
		align,
		left = "start",
		top = "start",
		right = "end",
		bottom = "end",
		align
	)

	parse_background = function(bg) {
		if (is.character(bg)) {
			bg = convert_color(bg)
		}
		if (!is.numeric(bg)) {
			stop("`background` must be numeric or a color string.")
		}
		if (length(bg) == 1) {
			bg = rep(bg, 3)
		}
		if (length(bg) == 3) {
			bg = c(bg, 1)
		}
		if (length(bg) != 4) {
			stop("`background` must be length 1, 3, or 4.")
		}
		if (!all(is.finite(bg)) || !all(bg >= 0 & bg <= 1)) {
			stop("`background` values must be in [0,1].")
		}
		bg
	}

	images = lapply(input_list, function(img) {
		ray_read_image(img, convert_to_array = TRUE)
	})

	base_image = images[[1]]
	base_colorspace = attr(base_image, "colorspace")
	base_white = attr(base_image, "white_current")

	if (convert_colorspace) {
		for (i in seq_along(images)) {
			images[[i]] = render_convert_colorspace(
				images[[i]],
				to_mats = base_colorspace,
				to_white = base_white
			)
		}
		base_white = attr(images[[1]], "white_current")
	}

	heights = vapply(images, function(img) dim(img)[1], numeric(1))
	widths = vapply(images, function(img) dim(img)[2], numeric(1))
	max_height = max(heights)
	max_width = max(widths)

	if (is_vertical) {
		out_height = sum(heights)
		out_width = max_width
	} else {
		out_height = max_height
		out_width = sum(widths)
	}

	bg = parse_background(background)
	stacked = array(0, dim = c(out_height, out_width, 4))
	stacked[,, 1] = bg[1]
	stacked[,, 2] = bg[2]
	stacked[,, 3] = bg[3]
	stacked[,, 4] = bg[4]

	align_offset = function(total, size) {
		extra = total - size
		if (extra < 0) {
			stop("stacked size cannot be smaller than component size.")
		}
		switch(
			align,
			start = 1L,
			center = floor(extra / 2) + 1L,
			end = extra + 1L
		)
	}

	if (is_vertical) {
		row_start = 1L
		for (i in seq_along(images)) {
			img = unclass(images[[i]])
			h = dim(img)[1]
			w = dim(img)[2]
			col_start = align_offset(max_width, w)
			rows = row_start:(row_start + h - 1L)
			cols = col_start:(col_start + w - 1L)
			stacked[rows, cols, ] = img
			row_start = row_start + h
		}
	} else {
		col_start = 1L
		for (i in seq_along(images)) {
			img = unclass(images[[i]])
			h = dim(img)[1]
			w = dim(img)[2]
			row_start = align_offset(max_height, h)
			rows = row_start:(row_start + h - 1L)
			cols = col_start:(col_start + w - 1L)
			stacked[rows, cols, ] = img
			col_start = col_start + w
		}
	}

	stacked = ray_read_image(
		stacked,
		assume_colorspace = base_colorspace,
		assume_white = base_white,
		source_linear = TRUE
	)
	handle_image_output(stacked, filename = filename, preview = preview)
}
