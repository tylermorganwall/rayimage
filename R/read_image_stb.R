#' Internal wrapper for stb_image C++ loader
#'
#' @description
#' Internal helper that loads an image via the C++ `load_image_stb()` function
#' and returns a 3D RGBA numeric array with dimensions `c(height, width, channels)`.
#'
#' @param filename Default `NULL`. Path to an image file to load.
#' @param desired_channels Default `4L`. Number of channels to request from the loader.
#'
#' @return A numeric 3D array (height, width, `desired_channels`).
#' @keywords internal
#' @noRd
read_image_stb = function(filename = NULL, desired_channels = 4L) {
  if (!is.character(filename) || length(filename) != 1L) {
    stop("'read_image_stb' expects a single filename string.")
  }

  filename = normalizePath(filename, winslash = "/", mustWork = TRUE)

  # Dummy values required by the Rcpp-exported interface
  width = 0L
  height = 0L
  channels = 0L

  img = load_image_stb(
    filename = filename,
    width = width,
    height = height,
    channels = channels,
    desired_channels = as.integer(desired_channels)
  )

  if (!is.numeric(img)) {
    stop("stb loader did not return a numeric array.")
  }
  if (is.null(dim(img)) || length(dim(img)) != 3L) {
    stop("stb loader did not return a 3D array.")
  }
  if (dim(img)[3L] != desired_channels) {
    stop(
      sprintf(
        "stb loader returned %d channels; expected %d.",
        dim(img)[3L],
        desired_channels
      )
    )
  }

  storage.mode(img) = "double"
  img
}
