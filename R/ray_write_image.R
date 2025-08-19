#'@title Write Image
#'
#'@description Takes an RGB array/filename and writes it to file.
#'
#'@param image 3-layer RGB/4-layer RGBA array, `rayimg` class, or filename of an image.
#'@param filename File to write to, with filetype determined by extension. Filetype can be
#'`PNG`, `JPEG`, `TIFF`, or `EXR`.
#'@param clamp Default `NA`, automatically determined. Whether to clamp the image to 0-1. If the file extension is `PNG` of `JPEG`,
#'this is forced to `TRUE`.
#'@param ... Arguments to pass to either `jpeg::writeJPEG`, `png::writePNG`, or `tiff::writeTIFF`.
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
ray_write_image = function(image, filename, clamp = NA, ...) {
  image = ray_read_image(image) #Always output RGBA array
  #Check if file or image before below:
  imagetype = get_file_type(image)
  #This isn't a check for rayimg type, it's just checking the base R type
  gamma_corrected = attr(image, "gamma_correct")

  if (!imagetype %in% c("array", "matrix")) {
    file.copy(image, filename)
  } else {
    fileext = tolower(tools::file_ext(filename))
    if (!fileext %in% c("png", "jpeg", "jpg", "tiff", "exr")) {
      stop(sprintf(
        "File extension (%s) must be one of `png`, `jpeg`, `jpg`, `exr`, or `tiff`",
        fileext
      ))
    }
    if (is.na(clamp)) {
      if (fileext %in% c("jpg", "jpeg", "png")) {
        clamp = TRUE
      } else {
        clamp = FALSE
      }
    }
    if (clamp || fileext %in% c("jpg", "jpeg", "png")) {
      image[image > 1] = 1
      image[image < 0] = 0
    }
    if (fileext %in% c("jpeg", "jpg")) {
      if (!gamma_corrected) {
        image[,, 1:3] = to_srgb(image[,, 1:3])
      }
      jpeg::writeJPEG(image, target = filename, ...)
    } else if (fileext == "png") {
      if (!gamma_corrected) {
        image[,, 1:3] = to_srgb(image[,, 1:3])
      }
      png::writePNG(image, target = filename, ...)
    } else if (fileext == "exr") {
      if (length(find.package("libopenexr", quiet = TRUE)) > 0) {
        libopenexr::write_exr(
          filename,
          r = image[,, 1],
          g = image[,, 2],
          b = image[,, 3],
          a = image[,, 4]
        )
      }
    } else {
      tiff::writeTIFF(image, where = filename, ...)
    }
  }
}
