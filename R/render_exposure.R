#' Apply exposure compensation (in stops)
#'
#' @param image              3-layer RGB/4-layer RGBA array, `rayimg` class, or filename of an image.
#' @param exposure           Default `0`. Exposure compensation in stops; RGB is scaled by `2^exposure`.
#' @param gamma_correction   Default `NA`. If `TRUE`, treat as sRGB (decode -> scale -> encode).
#'   If `FALSE`, treat as scene-linear/HDR and scale directly. If `NA`, auto-detect using
#'   `attr(image, "gamma_corrected")`, falling back to a range heuristic (outside 0..1 is linear).
#' @param filename           Default `NA`. Output path.
#' @param preview            Default `FALSE`. If `TRUE`, display the image.
#' @param ...                Additional args passed to [plot_image()] (when `preview=TRUE`)
#'   or to [ray_write_image()] (when `filename` is given).
#'
#' @return A `rayimg` RGBA array.
#' @export
#' @examples
#' # LDR/sRGB (auto): decodes to linear, applies EV, re-encodes
#' if (run_documentation()) {
#'   render_exposure(dragon, exposure = +1, preview = TRUE)
#' }
#' # Force linear/HDR behavior
#' if (run_documentation()) {
#'   render_exposure(dragon * 2, exposure = -1, gamma_correction = FALSE, preview = TRUE)
#' }
render_exposure = function(
  image,
  exposure = 0,
  gamma_correction = NA,
  filename = NA,
  preview = FALSE,
  ...
) {
  img = ray_read_image(image)
  scale = 2^exposure

  image_gamma_correct = attr(img, "gamma_corrected")
  do_gamma = if (is.na(gamma_correction)) {
    isTRUE(image_gamma_correct)
  } else {
    isTRUE(gamma_correction)
  }

  out = img
  if (length(dim(out)) == 2) {
    if (do_gamma) {
      lin = to_linear(out)
      out = to_srgb(lin * scale)
    } else {
      out = out * scale
    }
  } else {
    channels = dim(image)[3]
    if (channels == 2 || channels == 4) {
      max_channel = channels - 1
    } else {
      max_channel = channels
    }
    if (do_gamma) {
      for (ch in seq_len(max_channel)) {
        lin = to_linear(out[,, ch])
        out[,, ch] = to_srgb(lin * scale)
      }
    } else {
      out[,, seq_len(max_channel)] = out[,, seq_len(max_channel)] * scale
    }
  }

  attr(out, "gamma_corrected") = if (do_gamma) TRUE else {
    if (!is.null(image_gamma_correct)) image_gamma_correct else FALSE
  }

  handle_image_output(out, filename = filename, preview = preview)
}
