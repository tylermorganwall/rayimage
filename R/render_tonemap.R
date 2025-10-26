#' @title Render Tonemap
#'
#' @description Reinhard / Uncharted(Hable) / HBD operators in linear
#'
#' @param image Default `NULL`. 3/4-channel array, `rayimg`, or filename.
#' @param method Default `raw`. One of `"raw"`, `"reinhard"`, `"uncharted"`, `"hbd"`.
#' Chooses the global tone-mapping curve. `"raw"` leaves the image unchanged.
#' `"reinhard"` applies a classic shoulder that gently compresses highlights.
#' `"uncharted"` (Hable filmic) gives a film-style toe/shoulder and keeps mid-tones contrasty.
#' `"hbd"` is a fast filmic-like curve with a soft toe/shoulder.
#' All methods operate on **linear RGB** and return **display-referred linear** (no OETF).
#' @param exposure_bias Default `2`. Only for `"uncharted"`.
#' Scalar applied to the linear RGB channels **before** the Uncharted/Hable curve
#' (i.e., a pre-curve exposure gain). Values >1 brighten; <1 darken.
#' `exposure_bias=2` is about +1 stop; `0.5` is about −1 stop.
#' @param W Default `11.2`. White (linear scene value) that maps to 1.0 **after**
#' the Uncharted/Hable/Reinhard curve. Acts like a “white point” for the shoulder: larger values
#' preserve more highlight detail (later roll-off), smaller values roll off earlier and harder.
#' @param filename Default `NULL`. Output path.
#' @param preview Default `FALSE`. Show result.
#' @return A `rayimg` RGBA array.
#' @export
#' @examples
#' # Plot unchanged image
#' render_tonemap(dragon, preview = TRUE)
#' # Plot Reinhard tonemapped image
#' render_tonemap(dragon, method = "reinhard", preview = TRUE)
#' # Plot Uncharted/Hable tonemapped image
#' render_tonemap(dragon, method = "uncharted",preview = TRUE)
#' # Plot Uncharted/Hable tonemapped image, white point adjusted
#' render_tonemap(dragon, method = "uncharted", W=11.4, preview = TRUE)
#' # Plot Uncharted/Hable tonemapped image, exposure adjusted
#' render_tonemap(dragon, method = "uncharted", exposure_bias = 1,preview = TRUE)
#' # Plot Hejl-Burgess-Dawson tonemapped image, exposure adjusted
#' render_tonemap(dragon, method = "hbd",preview = TRUE)
render_tonemap = function(
	image,
	method = c("raw", "reinhard", "uncharted", "hbd"),
	exposure_bias = 2.0,
	W = 11.2,
	filename = NULL,
	preview = FALSE
) {
	method = match.arg(method)
	src = ray_read_image(image, convert_to_array = TRUE)
	colorspace_luminance_row = attr(src, "colorspace")$rgb_to_xyz[2, ]
	d = dim(src)
	if (length(d) != 3) {
		return(src)
	}

	rgb = src[,, 1:3]

	if (method == "reinhard") {
		eps = 1e-6
		lw = colorspace_luminance_row

		L = lw[1] * rgb[,, 1] + lw[2] * rgb[,, 2] + lw[3] * rgb[,, 3]

		a = 0.18
		L_log_avg = exp(mean(log(pmax(L, eps))))

		Lm = (a / L_log_avg) * L
		Lwhite = W #White point
		Ld = (Lm * (1 + (Lm / (Lwhite * Lwhite)))) / (1 + Lm)

		scale = Ld / pmax(L, eps)

		rgb[,, 1] = rgb[,, 1] * scale
		rgb[,, 2] = rgb[,, 2] * scale
		rgb[,, 3] = rgb[,, 3] * scale
	} else if (method == "uncharted") {
		A = 0.15
		B = 0.50
		C = 0.10
		D = 0.20
		E = 0.02
		F = 0.30
		eps = 1e-6

		lw = colorspace_luminance_row
		L = lw[1] * rgb[,, 1] + lw[2] * rgb[,, 2] + lw[3] * rgb[,, 3]

		# Key normalization
		a = 0.18
		L_log_avg = exp(mean(log(pmax(L, eps))))
		pre_key = a / L_log_avg

		hable = function(x) {
			((x * (A * x + C * B) + D * E) / (x * (A * x + B) + D * F) - E / F)
		}

		e = exposure_bias
		whiteScale = 1 / hable(e * W)

		Lm = e * pre_key * L
		Ld = hable(Lm) * whiteScale
		scale = Ld / pmax(L, eps)

		rgb[,, 1] = rgb[,, 1] * scale
		rgb[,, 2] = rgb[,, 2] * scale
		rgb[,, 3] = rgb[,, 3] * scale
	} else if (method == "hbd") {
		for (k in 1:3) {
			x = pmax(rgb[,, k] - 0.004, 0)
			rgb[,, k] = (x * (6.2 * x + 0.5)) / (x * (6.2 * x + 1.7) + 0.06)
		}
	} else {
		# Nothing
	}

	out = src
	out[,, 1:3] = rgb

	out = ray_read_image(out)
	attributes(out) = attributes(src)
	handle_image_output(out, filename = filename, preview = preview)
}
