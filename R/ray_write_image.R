#'@title Write Image
#'
#'@description Takes an RGB array/filename and writes it to file.
#'
#'@param image 3-layer RGB/4-layer RGBA array, `rayimg` class, or filename of an image.
#'@param filename File to write to, with filetype determined by extension. Filetype can be
#'`PNG`, `JPEG`, `TIFF`, or `EXR`.
#'@param clamp Default `FALSE`, automatically determined. Whether to clamp the image to 0-1. If the file extension is `PNG` of `JPEG`,
#'this is forced to `TRUE`.
#'@param write_linear Default `NA`, automatically determined. By default, images will be gamma corrected (write_linear = FALSE) for
#' all file types except `EXR` (which is a linear format), unless otherwise specified.
#'@param ... Arguments to pass to either `jpeg::writeJPEG`, `png::writePNG`, `libopenexr::write_exr`, or `tiff::writeTIFF`.
#'@return A `rayimg` RGBA array.
#'@import grDevices
#'@export
#'@examples
#'if(run_documentation()){
#'#Write as a png
#'tmparr = tempfile(fileext=".png")
#'ray_read_image(dragon) |>
#'  ray_write_image(tmparr)
#'ray_read_image(tmparr) |>
#'  plot_image()
#'}
#'if(run_documentation()){
#'#Write as a JPEG (passing quality arguments via ...)
#'tmparr = tempfile(fileext=".jpg")
#'ray_read_image(dragon) |>
#'  ray_write_image(tmparr, quality = 0.2)
#'ray_read_image(tmparr) |>
#'  plot_image()
#'}
#'if(run_documentation()){
#'#Write as a tiff
#'tmparr = tempfile(fileext=".tiff")
#'ray_read_image(dragon) |>
#'  ray_write_image(tmparr)
#'ray_read_image(tmparr) |>
#'   plot_image()
#'}
ray_write_image = function(
  image,
  filename,
  clamp = FALSE,
  write_linear = NA,
  ...
) {
  if (missing(filename)) {
    stop("`filename` must be specified.")
  }
  image = ray_read_image(image) #Always output RGBA array
  #Image should always be linear by this point

  fileext = tolower(tools::file_ext(filename))
  if (!fileext %in% c("png", "jpeg", "jpg", "tiff", "exr")) {
    stop(sprintf(
      "File extension (%s) must be one of `png`, `jpeg`, `jpg`, `exr`, or `tiff`",
      fileext
    ))
  }
  if (clamp || fileext %in% c("png", "jpeg", "jpg", "tiff")) {
    image = render_clamp(image)
  }
  if (is.na(write_linear)) {
    write_linear = !(fileext %in% c("png", "jpeg", "jpg", "tiff"))
  }
  is_matrix = length(dim(image)) == 2
  if (is_matrix) {
    if (!write_linear) {
      image = to_srgb(image)
    }
  } else {
    max_dim = dim(image)[3L]
    has_alpha = max_dim %in% c(2L, 4L)
    if (has_alpha) {
      col_dims = seq_len(max_dim - 1L)
      alpha_channel = image[,, max_dim]
      alpha_channel[alpha_channel > 1] = 1
      alpha_channel[alpha_channel < 0] = 0
      image[,, max_dim] = alpha_channel
    } else {
      col_dims = seq_len(max_dim)
    }
    if (!write_linear) {
      image[,, col_dims] = to_srgb(image[,, col_dims])
    }
  }
  if (fileext %in% c("jpeg", "jpg")) {
    jpeg::writeJPEG(image, target = filename, ...)
  } else if (fileext == "png") {
    png::writePNG(image, target = filename, ...)
  } else if (fileext == "exr") {
    if (length(find.package("libopenexr", quiet = TRUE)) > 0) {
      rgba = array(1, dim = c(dim(image)[1:2], 4))
      if (is_matrix) {
        rgba[,, 1:3] = image
      } else if (dim(image)[3] == 2) {
        rgba[,, 1] = image[,, 1]
        rgba[,, 2] = image[,, 1]
        rgba[,, 3] = image[,, 1]
        rgba[,, 4] = image[,, 2]
      } else if (dim(image)[3] == 3) {
        rgba[,, 1] = image[,, 1]
        rgba[,, 2] = image[,, 2]
        rgba[,, 3] = image[,, 3]
      } else if (dim(image)[3] == 4) {
        rgba = image
      }
      libopenexr::write_exr(
        filename,
        r = rgba[,, 1],
        g = rgba[,, 2],
        b = rgba[,, 3],
        a = rgba[,, 4]
      )
    }
  } else {
    tiff::writeTIFF(image, where = filename, ...)
  }
}
