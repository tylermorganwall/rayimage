#' @title Render Vibrance
#'
#' @description
#' Adjusts image vibrance (adaptive saturation), similar in spirit to the iPhone "Vibrance" control:
#' it boosts muted colors more than already-saturated colors, and reduces saturation more strongly
#' in already-saturated regions when negative. Works on HDR arrays as well (values can exceed 1).
#'
#' @param image Image filename, 3-layer RGB array, 4-layer RGBA array, or matrix. If a filename,
#' it will be read via `ray_read_image()`.
#' @param vibrance Default `0`. Numeric scalar in -1..1 (values are clamped). Positive values
#' increase vibrance primarily in low-saturation regions; negative values decrease vibrance primarily
#' in high-saturation regions.
#' @param protect_luminance Default `0.25`. Numeric scalar in 0..1. Reduces the vibrance effect
#' in deep shadows and bright highlights to avoid harsh color shifts. Set to `0` to disable.
#' @param filename Default `NULL`. If not `NULL`, the processed image will be written to this file.
#' @param preview Default `FALSE`. Whether to plot the processed image.
#'
#' @return 4-layer RGBA array (or matrix if passed a matrix and `ray_read_image()` is not used).
#' @export
#'
#' @examples
#' if(run_documentation()){
#'   dragon |>
#'     render_vibrance(vibrance = 0.4, preview = TRUE)
#' }
render_vibrance = function(
	image,
	vibrance = 0,
	protect_luminance = 0.25,
	filename = NULL,
	preview = FALSE
) {
	if (length(vibrance) != 1 || !is.numeric(vibrance) || !is.finite(vibrance)) {
		stop("`vibrance` must be a single finite numeric value.")
	}
	if (
		length(protect_luminance) != 1 ||
			!is.numeric(protect_luminance) ||
			!is.finite(protect_luminance)
	) {
		stop("`protect_luminance` must be a single finite numeric value.")
	}
	if (protect_luminance < 0 || protect_luminance > 1) {
		stop("`protect_luminance` must be in [0,1].")
	}

	vibrance = max(min(vibrance, 1), -1)

	temp_image = ray_read_image(image) # returns RGBA array in rayimage

	nr = dim(temp_image)[1]
	nc = dim(temp_image)[2]

	r = as.vector(temp_image[,, 1])
	g = as.vector(temp_image[,, 2])
	b = as.vector(temp_image[,, 3])

	na_mask = is.na(r) | is.na(g) | is.na(b)
	if (any(na_mask)) {
		r[na_mask] = 0
		g[na_mask] = 0
		b[na_mask] = 0
	}

	# Avoid negative values breaking HSV math; keep HDR highs intact.
	r = pmax(r, 0)
	g = pmax(g, 0)
	b = pmax(b, 0)

	maxc = pmax(r, pmax(g, b))
	minc = pmin(r, pmin(g, b))
	delta = maxc - minc

	v = maxc
	s = rep(0, length(v))
	nz = maxc > 0
	s[nz] = delta[nz] / maxc[nz]

	h = rep(0, length(v))
	idx = delta > 0

	if (any(idx)) {
		idx_r = idx & (maxc == r)
		idx_g = idx & (maxc == g) & !idx_r
		idx_b = idx & (maxc == b) & !(idx_r | idx_g)

		if (any(idx_r)) {
			h[idx_r] = (g[idx_r] - b[idx_r]) / delta[idx_r]
			h[idx_r][h[idx_r] < 0] = h[idx_r][h[idx_r] < 0] + 6
		}
		if (any(idx_g)) {
			h[idx_g] = (b[idx_g] - r[idx_g]) / delta[idx_g] + 2
		}
		if (any(idx_b)) {
			h[idx_b] = (r[idx_b] - g[idx_b]) / delta[idx_b] + 4
		}
		h[idx] = h[idx] / 6
		h = h - floor(h) # wrap into [0,1)
	}

	if (protect_luminance > 0) {
		v01 = pmin(v, 1)
		w = 1 - protect_luminance * abs(2 * v01 - 1)
		w = pmax(w, 0)
	} else {
		w = 1
	}

	if (vibrance >= 0) {
		s2 = s + vibrance * (1 - s) * w
	} else {
		s2 = s + vibrance * s * w
	}
	s2 = pmin(pmax(s2, 0), 1)

	# HSV -> RGB (vectorized), preserving HDR v
	r2 = v
	g2 = v
	b2 = v

	sat = s2
	idx_sat = sat > 0

	if (any(idx_sat)) {
		h6 = h * 6
		i = floor(h6)
		f = h6 - i

		p = v * (1 - sat)
		q = v * (1 - sat * f)
		t = v * (1 - sat * (1 - f))

		im = i %% 6

		k0 = idx_sat & (im == 0)
		k1 = idx_sat & (im == 1)
		k2 = idx_sat & (im == 2)
		k3 = idx_sat & (im == 3)
		k4 = idx_sat & (im == 4)
		k5 = idx_sat & (im == 5)

		if (any(k0)) {
			r2[k0] = v[k0]
			g2[k0] = t[k0]
			b2[k0] = p[k0]
		}
		if (any(k1)) {
			r2[k1] = q[k1]
			g2[k1] = v[k1]
			b2[k1] = p[k1]
		}
		if (any(k2)) {
			r2[k2] = p[k2]
			g2[k2] = v[k2]
			b2[k2] = t[k2]
		}
		if (any(k3)) {
			r2[k3] = p[k3]
			g2[k3] = q[k3]
			b2[k3] = v[k3]
		}
		if (any(k4)) {
			r2[k4] = t[k4]
			g2[k4] = p[k4]
			b2[k4] = v[k4]
		}
		if (any(k5)) {
			r2[k5] = v[k5]
			g2[k5] = p[k5]
			b2[k5] = q[k5]
		}
	}

	if (any(na_mask)) {
		r2[na_mask] = NA_real_
		g2[na_mask] = NA_real_
		b2[na_mask] = NA_real_
	}

	temp_image[,, 1] = matrix(r2, nrow = nr, ncol = nc)
	temp_image[,, 2] = matrix(g2, nrow = nr, ncol = nc)
	temp_image[,, 3] = matrix(b2, nrow = nr, ncol = nc)

	handle_image_output(temp_image, filename = filename, preview = preview)
}
