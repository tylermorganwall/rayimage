#'@title Write Image
#'
#'@description Takes an RGB array/filename and writes it to file.
#'
#'@param image Image filename or 3-layer RGB array.
#'@param filename File to write to, with filetype determined by extension. Filetype can be
#'`PNG`, `JPEG`, or `TIFF`.
#'@param clamp Default `TRUE`. Whether to clamp the image to 0-1. If the file extension is `PNG` of `JPEG`,
#'this is forced to `TRUE`.
#'@param ... Arguments to pass to either `jpeg::writeJPEG`, `png::writePNG`, or `tiff::writeTIFF`.
#'@return 3-layer RGB array of the processed image.
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
ray_write_image = function(image, filename, clamp = TRUE, ...) {
  imagetype = get_file_type(image)
  if(!imagetype %in% c("array", "matrix")) {
    file.copy(image, filename)
  } else {
    fileext = tolower(tools::file_ext(filename))
    if(!fileext %in% c("png","jpeg","jpg","tiff")) {
      stop(sprintf("File extension (%s) must be one of `png`, `jpeg`, `jpg`, or `tiff`",fileext))
    }

    if(clamp || fileext %in% c("jpg", "jpeg", "png")) {
      image[image > 1] = 1
      image[image < 0] = 0
    }
    if(fileext %in% c("jpeg","jpg")) {
      jpeg::writeJPEG(image, target = filename, ...)
    } else if (fileext == "png") {
      png::writePNG(image, target = filename, ...)
    } else {
      tiff::writeTIFF(image, where = filename, ...)
    }
  }
}
