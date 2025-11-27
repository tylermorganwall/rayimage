#' @title Select PNG Device
#'
#' @description Internal helper to pick the best available PNG graphics
#' device. Prefers `ragg::agg_png()` when available, then falls back to a
#' Cairo-backed `grDevices::png()` device, and finally to the base PNG
#' device.
#'
#' @param use_ragg Logical. If `TRUE` (default) and the `ragg` package is
#' available, return `ragg::agg_png()`. Set to `FALSE` to skip `ragg` even if
#' installed.
#'
#' @keywords internal
linear_png_device = function(use_ragg = TRUE) {
	png_device = grDevices::png
	if (use_ragg && requireNamespace("ragg", quietly = TRUE)) {
		png_device = function(...) {
			args = list(...)
			args$family = NULL
			do.call(ragg::agg_png, args)
		}
	} else if (isTRUE(capabilities("cairo"))) {
		png_device = function(...) grDevices::png(..., type = "cairo")
	} else {
		warning(
			"No cairo device available: Install the {ragg} package to ensure correct treatment of gamma when adding text to images."
		)
	}
	return(png_device)
}
