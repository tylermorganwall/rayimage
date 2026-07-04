#' Estimate Lat-Long Environment White
#'
#' @description Estimate the scene white produced by a lat-long environment map.
#' The default weighting matches the irradiance seen by an upward-facing diffuse
#' surface: `cos(theta) * d_omega` over the upper hemisphere.
#'
#' @param environment_map Lat-long environment map image, `rayimg`, array, or filename.
#' @param weighting Default `"cosine"`. Weighting method. `"cosine"` computes
#'   upward diffuse irradiance; `"solid_angle"` computes an unprojected spherical
#'   average.
#' @param hemisphere Default `"upper"`. Hemisphere to use. `"upper"` uses only
#'   rows above the horizon; `"full"` uses the full lat-long map.
#' @param environment_colorspace Default `NULL`. Optional colorspace descriptor
#'   used to tag `environment_map` on read.
#' @param environment_white Default `NULL`. Optional white point used to tag
#'   `environment_map` on read.
#'
#' @return Length-3 XYZ white point normalized to `Y = 1`. The returned vector
#'   includes `rgb`, `rgb_ratio`, `xy`, approximate `cct`, `sum_weights`,
#'   `weighting`, and `hemisphere` attributes for diagnostics.
#' @export
estimate_environment_white = function(
  environment_map,
  weighting = c("cosine", "solid_angle"),
  hemisphere = c("upper", "full"),
  environment_colorspace = NULL,
  environment_white = NULL
) {
  weighting = match.arg(weighting)
  hemisphere = match.arg(hemisphere)

  env = ray_read_image(
    environment_map,
    normalize = FALSE,
    assume_colorspace = environment_colorspace,
    assume_white = environment_white
  )
  d = dim(env)
  if (length(d) != 3L || d[3] < 3L) {
    stop(
      "estimate_environment_white(): environment_map must have RGB channels."
    )
  }

  colorspace = attr(env, "colorspace", exact = TRUE)
  if (!is.list(colorspace) || is.null(colorspace$rgb_to_xyz)) {
    stop(
      "estimate_environment_white(): environment_map is missing colorspace metadata."
    )
  }

  n_theta = d[1]
  n_phi = d[2]
  theta = ((seq_len(n_theta) - 0.5) / n_theta) * pi
  d_theta = pi / n_theta
  d_phi = 2 * pi / n_phi

  weights = sin(theta) * d_theta * d_phi
  if (weighting == "cosine") {
    weights = weights * pmax(cos(theta), 0)
  }
  if (hemisphere == "upper") {
    weights[theta > pi / 2] = 0
  }

  row_weight_sum = sum(weights)
  if (!is.finite(row_weight_sum) || row_weight_sum <= 0) {
    stop("estimate_environment_white(): environment weights sum to zero.")
  }

  weight_matrix = matrix(weights, nrow = n_theta, ncol = n_phi)
  sum_weights = sum(weight_matrix)
  rgb = vapply(
    seq_len(3L),
    function(channel) {
      sum(env[,, channel] * weight_matrix, na.rm = TRUE)
    },
    numeric(1)
  )

  if (!all(is.finite(rgb)) || sum(rgb) <= 0) {
    stop(
      "estimate_environment_white(): estimated environment RGB is not positive and finite."
    )
  }

  xyz = as.vector(colorspace$rgb_to_xyz %*% rgb)
  if (!all(is.finite(xyz)) || xyz[2] <= 0) {
    stop(
      "estimate_environment_white(): estimated environment XYZ is not positive and finite."
    )
  }

  xyz = xyz / xyz[2]
  xy = c(xyz[1], xyz[2]) / sum(xyz)
  names(xyz) = c("X", "Y", "Z")
  attr(xyz, "rgb") = rgb
  attr(xyz, "rgb_ratio") = rgb / mean(rgb)
  attr(xyz, "xy") = xy
  attr(xyz, "cct") = xy_to_cct_mccamy(xy)
  attr(xyz, "sum_weights") = sum_weights
  attr(xyz, "weighting") = weighting
  attr(xyz, "hemisphere") = hemisphere
  xyz
}

#' White Balance with a Lat-Long Environment Map
#'
#' @description Estimate a lat-long environment map's scene white and pass it to
#' [render_white_balance()] as `reference_white`. This is a camera-style white
#' balance for renders lit by an environment map.
#'
#' @param image 3-layer RGB/4-layer RGBA array, `rayimg`, or filename.
#' @param environment_map Lat-long environment map image, `rayimg`, array, or filename.
#' @param target_white Default `"D65"`. Target white (XYZ Y=1 or named).
#' @param bake Default `TRUE`. Passed to [render_white_balance()].
#' @param weighting Default `"cosine"`. Passed to the environment white
#'   estimator.
#' @param hemisphere Default `"upper"`. Passed to the environment white
#'   estimator.
#' @param environment_colorspace Default `NULL`. Optional colorspace descriptor
#'   used to tag `environment_map` on read.
#' @param environment_white Default `NULL`. Optional white point used to tag
#'   `environment_map` on read.
#' @param filename Default `NULL`. Output path.
#' @param preview Default `FALSE`. If `TRUE`, display the image.
#'
#' @return A white-balanced `rayimg`. The estimated white is attached as the
#'   `environment_white` attribute.
#' @export
render_environment_white_balance = function(
  image,
  environment_map,
  target_white = "D65",
  bake = TRUE,
  weighting = "cosine",
  hemisphere = "upper",
  environment_colorspace = NULL,
  environment_white = NULL,
  filename = NULL,
  preview = FALSE
) {
  reference_white = estimate_environment_white(
    environment_map,
    weighting = weighting,
    hemisphere = hemisphere,
    environment_colorspace = environment_colorspace,
    environment_white = environment_white
  )

  out = render_white_balance(
    image,
    reference_white = reference_white,
    target_white = target_white,
    bake = bake,
    filename = filename,
    preview = preview
  )
  attr(out, "environment_white") = reference_white
  out
}

#' Estimate correlated color temperature from xy chromaticity
#' @param xy xy chromaticity coordinates.
#' @return Approximate correlated color temperature in Kelvin.
#' @keywords internal
#' @noRd
xy_to_cct_mccamy = function(xy) {
  n = (xy[1] - 0.3320) / (0.1858 - xy[2])
  449 * n^3 + 3525 * n^2 + 6823.3 * n + 5520.33
}
