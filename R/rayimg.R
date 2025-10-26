#' Create a rayimg
#'
#' This class will always be:
#'  * A matrix or a 2/3/4 layer array
#'  * It tracks whether the source data was linearized
#'  * It carries working colorspace/white
#'
#' @param x Default `NULL`. The underlying array/matrix to wrap.
#' @param filetype Default `NULL`. Original source type to record (e.g., "png").
#' @param source_linear Default `FALSE`. Whether the original source was linearized.
#' @param colorspace Default `CS_ACESCG`. Working space descriptor (see `make_colorspace()`).
#' @param white_current Default `colorspace$white_xyz`. Current assumed scene/display white (XYZ, Y=1).
#'
#' @return An object of class `c("rayimg", <original class>)` with attributes.
#' @keywords internal
rayimg = function(
	x = NULL,
	filetype = NULL,
	source_linear = FALSE,
	colorspace = CS_ACESCG,
	white_current = colorspace$white_xyz
) {
	if (is.null(x)) {
		stop("rayimg(): 'x' cannot be NULL.")
	}
	stopifnot(is.array(x))

	x_new = rayimg_validate_dimensions(x)

	# channels attribute
	if (is.null(attr(x, "channels"))) {
		attr(x_new, "channels") = rayimg_detect_channels(x_new)
	} else {
		attr(x_new, "channels") = attr(x, "channels")
	}

	# required attrs
	attr(x_new, "filetype") = filetype
	attr(x_new, "source_linear") = source_linear

	# new attrs for color mgmt
	stopifnot(
		is.list(colorspace),
		all(c("rgb_to_xyz", "xyz_to_rgb", "white_xyz") %in% names(colorspace))
	)
	attr(x_new, "colorspace") = colorspace
	attr(x_new, "white_current") = white_current
	class(x_new) = c("rayimg", setdiff(class(x), "rayimg"))
	x_new
}

rayimg_channels_from_count = function(n) {
	switch(
		as.character(n),
		"1" = c("Greyscale"),
		"2" = c("Greyscale", "Alpha"),
		"3" = c("Red", "Green", "Blue"),
		"4" = c("Red", "Green", "Blue", "Alpha"),
		stop("This should never be hit.")
	)
}

rayimg_detect_channels = function(x) {
	d = dim(x)
	if (length(d) < 3L) {
		return("Greyscale")
	}
	rayimg_channels_from_count(d[3])
}

#Always return a multidimensional (d[3] = 2,3,4) array, or a matrix.
rayimg_validate_dimensions = function(x) {
	d = dim(x)
	stopifnot(!is.null(d), length(d) %in% c(2, 3), d[1] > 0, d[2] > 0)
	if (length(d) == 3) {
		if (d[3] == 1) {
			x = x[,, 1] # drop extra
		} else {
			stopifnot(d[3] %in% c(2, 3, 4))
		}
	}
	x
}


#' Create a rayimg
#'
#' @param x Default `NULL`. Underlying array/matrix to wrap.
#' @param filetype Default `NULL`. Original source type (e.g., "png").
#' @param source_linear Default `FALSE`. Whether the original source was linearized.
#' @param colorspace Default `CS_ACESCG`. Working space descriptor.
#' @param white_current Default `colorspace$white_xyz`. Current white (XYZ, Y=1).
#'
#' @return `rayimg`
#' @keywords internal
rayimg = function(
	x = NULL,
	filetype = NULL,
	source_linear = FALSE,
	colorspace = CS_ACESCG,
	white_current = colorspace$white_xyz
) {
	if (is.null(x)) {
		stop("rayimg(): 'x' cannot be NULL.")
	}
	stopifnot(is.array(x))

	x_new = rayimg_validate_dimensions(x)

	if (is.null(attr(x, "channels"))) {
		attr(x_new, "channels") = rayimg_detect_channels(x_new)
	} else {
		attr(x_new, "channels") = attr(x, "channels")
	}

	attr(x_new, "filetype") = filetype
	attr(x_new, "source_linear") = source_linear

	stopifnot(
		is.list(colorspace),
		all(c("rgb_to_xyz", "xyz_to_rgb", "white_xyz") %in% names(colorspace))
	)
	attr(x_new, "colorspace") = colorspace
	attr(x_new, "white_current") = white_current
	class(x_new) = c("rayimg", setdiff(class(x), "rayimg"))
	x_new
}

#' @export
`[.rayimg` = function(x, ..., drop = TRUE) {
	ch = rayimg_detect_channels(x)
	caller_env = parent.frame()
	mc = match.call(expand.dots = FALSE)
	dots_expr = as.list(mc$`...`)
	nd = length(dim(x))

	if (length(dots_expr) < nd) {
		dots_expr = c(dots_expr, rep(list(quote(expr = )), nd - length(dots_expr)))
	}
	idx = lapply(dots_expr, function(z) {
		if (identical(z, quote(expr = ))) TRUE else eval(z, envir = caller_env)
	})

	if (nd >= 3L) {
		if (!isTRUE(idx[[3L]]) && length(idx[[3L]]) > 4L) {
			stop("A rayimg can only have four or fewer channels selected at once.")
		}
		if (!isTRUE(idx[[3L]]) && any(idx[[3L]] > 4L)) {
			stop("rayimg: channel index > 4.")
		}
	}

	y = NextMethod("[", drop = FALSE)

	if (drop) {
		d = dim(y)
		dn = dimnames(y)
		drop_idx = which(d == 1L & seq_along(d) >= 3L)
		if (length(drop_idx)) {
			keep = setdiff(seq_along(d), drop_idx)
			y = array(y, dim = d[keep], dimnames = dn[keep])
		}
	}

	# carry attrs
	attr(y, "filetype") = attr(x, "filetype")
	attr(y, "source_linear") = attr(x, "source_linear")
	attr(y, "colorspace") = attr(x, "colorspace")
	attr(y, "white_current") = attr(x, "white_current")

	if (!is.null(ch)) {
		if (nd >= 3L) {
			ch_idx = idx[[3L]]
			if (isTRUE(ch_idx)) {
				ch_idx = seq_along(ch)
			}
			attr(y, "channels") = ch[ch_idx]
		} else {
			attr(y, "channels") = ch[seq_len(min(1L, length(ch)))]
		}
	} else {
		attr(y, "channels") = rayimg_detect_channels(y)
	}

	if (!is.null(dim(y))) {
		class(y) = unique(c("rayimg", setdiff(class(y), "rayimg")))
	}
	y
}
