#' Apply exposure compensation (in stops)
#'
#' @param image              3-layer RGB/4-layer RGBA array, `rayimg` class, or filename of an image.
#' @param exposure           Default `0`. Exposure compensation in stops; RGB is scaled by `2^exposure`.
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
#'   render_exposure(dragon * 2, exposure = -1, preview = TRUE)
#' }
render_exposure = function(
  image,
  exposure = 0,
  filename = NA,
  preview = FALSE,
  ...
) {
  img = ray_read_image(image)
	#This should always be linear from here down
  scale = 2^exposure

  out = img
  if (length(dim(out)) == 2) {
		out = out * scale
  } else {
    channels = dim(image)[3]
    if (channels == 2 || channels == 4) {
      max_channel = channels - 1
    } else {
      max_channel = channels
    }
		out[,, seq_len(max_channel)] = out[,, seq_len(max_channel)] * scale
  }

	out = ray_read_image(out)
  handle_image_output(out, filename = filename, preview = preview)
}
