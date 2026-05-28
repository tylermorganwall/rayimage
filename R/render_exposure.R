#' Apply exposure compensation and ISO gain
#'
#' @param image              3-layer RGB/4-layer RGBA array, `rayimg` class, or filename of an image.
#' @param exposure           Default `0`. Exposure compensation in stops; applied directly to RGB values.
#' @param iso                Default `NA`. ISO gain to apply directly to RGB values. `NA` applies no ISO gain
#'   and preserves the current informational ISO metadata.
#' @param auto               Default `FALSE`. If `TRUE`, automatically add the exposure needed to
#'   place a luminance percentile at 1.0 for LDR display/output. `exposure`
#'   is then treated as an additional compensation value.
#' @param percentile         Default `0.99`. Luminance percentile used when `auto = TRUE`.
#'   Must be greater than 0 and less than or equal to 1. Use `1` for max luminance.
#' @param verbose            Default `FALSE`. If `TRUE` and `auto = TRUE`, print the
#'   automatically computed exposure adjustment.
#' @param filename           Default `NA`. Output path.
#' @param preview            Default `FALSE`. If `TRUE`, display the image.
#' @param ...                Additional args passed to [plot_image()] (when `preview=TRUE`)
#'   or to [ray_write_image()] (when `filename` is given).
#'
#' @return A `rayimg` RGBA array.
#' @export
#'@examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#' # LDR/sRGB (auto): decodes to linear, applies EV, records camera metadata
#'   render_exposure(dragon, exposure = +1, preview = TRUE)
#' # Force linear/HDR behavior
#'   render_exposure(dragon * 2, exposure = -1, preview = TRUE)
render_exposure = function(
  image,
  exposure = 0,
  iso = NA,
  auto = FALSE,
  percentile = 0.99,
  verbose = FALSE,
  filename = NA,
  preview = FALSE,
  ...
) {
  img = ray_read_image(image)
  exposure = rayimg_exposure_value(exposure)
  if (length(iso) != 1L) {
    stop("render_exposure(): 'iso' must be a single positive number or NA.")
  }
  iso_supplied = !is.na(iso)
  if (iso_supplied) {
    iso_to_apply = rayimg_iso_value(iso)
    iso_metadata = iso_to_apply
  } else {
    iso_to_apply = 100
    iso_metadata = rayimg_iso_value(attr(img, "iso", exact = TRUE))
  }
  if (!is.logical(auto) || length(auto) != 1L || is.na(auto)) {
    stop("render_exposure(): 'auto' must be TRUE or FALSE.")
  }
  if (
    !is.numeric(percentile) ||
      length(percentile) != 1L ||
      is.na(percentile) ||
      !is.finite(percentile) ||
      percentile <= 0 ||
      percentile > 1
  ) {
    stop("render_exposure(): 'percentile' must be a number in (0, 1].")
  }
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("render_exposure(): 'verbose' must be TRUE or FALSE.")
  }

  current_exposure = rayimg_exposure_value(attr(img, "exposure", exact = TRUE))
  auto_exposure = 0
  target_luminance = NA_real_
  if (auto) {
    img_values = unclass(img)
    d = dim(img_values)
    if (length(d) == 2L) {
      luminance_values = as.numeric(img_values)
    } else {
      channels = d[3L]
      color_channels = if (channels %in% c(2L, 4L)) {
        seq_len(channels - 1L)
      } else {
        seq_len(channels)
      }
      if (length(color_channels) == 1L) {
        luminance_values = as.numeric(img_values[,, color_channels])
      } else {
        rgb_values = img_values[,, color_channels, drop = FALSE]
        if (length(color_channels) >= 3L) {
          rgb_values = rgb_values[,, seq_len(3L), drop = FALSE]
          cs = attr(img, "colorspace", exact = TRUE)
          luma_weights = if (
            is.list(cs) &&
              is.matrix(cs$rgb_to_xyz) &&
              all(dim(cs$rgb_to_xyz) >= c(2L, 3L))
          ) {
            as.numeric(cs$rgb_to_xyz[2L, seq_len(3L)])
          } else {
            c(0.2126, 0.7152, 0.0722)
          }
          luminance_values =
            rgb_values[,, 1L] *
            luma_weights[1L] +
            rgb_values[,, 2L] * luma_weights[2L] +
            rgb_values[,, 3L] * luma_weights[3L]
        } else {
          luminance_values = as.numeric(rgb_values)
        }
      }
    }
    luminance_values = luminance_values[is.finite(luminance_values)]
    if (length(luminance_values)) {
      target_luminance = as.numeric(stats::quantile(
        luminance_values,
        probs = percentile,
        names = FALSE
      ))
      current_scale = iso_to_apply / 100
      if (target_luminance > 0 && current_scale > 0) {
        auto_exposure = -log2(target_luminance * current_scale)
        exposure = exposure + auto_exposure
      }
    }
    if (verbose) {
      message(sprintf(
        "Auto exposure: %+.3f EV (luminance p%.3g: %.6g, ISO: %.6g)",
        auto_exposure,
        percentile,
        target_luminance,
        iso_to_apply
      ))
    }
  }
  scale = (2^exposure) * (iso_to_apply / 100)
  out_values = unclass(img)
  if (!isTRUE(all.equal(scale, 1))) {
    d = dim(out_values)
    if (length(d) == 2L) {
      out_values = out_values * scale
    } else {
      channels = d[3L]
      color_channels = if (channels %in% c(2L, 4L)) {
        seq_len(channels - 1L)
      } else {
        seq_len(channels)
      }
      out_values[,, color_channels] = out_values[,, color_channels] * scale
    }
  }

  out = rayimg(
    out_values,
    filetype = attr(img, "filetype", exact = TRUE),
    source_linear = attr(img, "source_linear", exact = TRUE),
    colorspace = attr(img, "colorspace", exact = TRUE),
    white_current = attr(img, "white_current", exact = TRUE),
    exposure = current_exposure + exposure,
    iso = iso_metadata
  )
  out = ray_read_image(out)
  handle_image_output(out, filename = filename, preview = preview)
}
