#'@title Read Image
#'
#'@description Reads an image from a file/array/matrix. From files, supports `JPEG`, `PNG`, `TIFF`, and `EXR` images.
#'
#'@param image 3-layer RGB/4-layer RGBA array, `rayimg` class, or filename of an image.
#'@param convert_to_array Default `TRUE`. Whether to convert 2D B&W images/matrices to RGBA arrays.
#'@param preview Default `FALSE`. If `TRUE`, it will display the image in addition
#'to returning it.
#'@param gamma_correct Default `NA`, automatically determined based on the image type.
#'Whether to label the image as gamma corrected.
#'@param ... Arguments to pass to either `jpeg::readJPEG`, `png::readPNG`, `tiff::readTIFF`, or `libopenexr::read_exr()`.
#'
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
#'
#'if(run_documentation()){
#'#Write as an exr
#'tmparr = tempfile(fileext=".exr")
#'ray_read_image(dragon) |>
#'  ray_write_image(tmparr)
#'ray_read_image(tmparr) |>
#'   plot_image()
#'}
ray_read_image = function(
  image,
  convert_to_array = FALSE,
  preview = FALSE,
  gamma_correct = NA,
  ...
) {
  process_image_preview = function(image2) {
    if (convert_to_array) {
      image_val = array(1, dim = c(dim(image2)[1:2], 4))
      if (length(dim(image2)) == 2) {
        image_val[,, 1:3] = image2
      } else if (length(dim(image2)) == 3) {
        if (dim(image2)[3] == 2) {
          image_val[,, 1:3] = image2[,, 1]
          image_val[,, 4] = image2[,, 2]
        } else if (dim(image2)[3] == 3) {
          image_val[,, 1:3] = image2[,, 1:3]
        } else if (dim(image2)[3] == 4) {
          image_val = image2
        }
      }
    } else {
      image_val = image2
    }
    if (preview) {
      plot_image(image2)
    }
    return(image2)
  }

  # Return immediately if already loaded
  imagetype = get_file_type(image)
  if (is.na(gamma_correct)) {
    if (imagetype %in% c("png", "jpg")) {
      gamma_correct = TRUE
    } else {
      gamma_correct = FALSE
    }
  }
  if (inherits(image, "rayimg")) {
    gamma_correct = attr(image, "gamma_corrected")
    filetype = attr(image, "filetype")
    return(rayimg(process_image_preview(image), filetype, gamma_correct))
  }
  if (imagetype == "array") {
    return(rayimg(process_image_preview(image), imagetype, gamma_correct))
  } else if (imagetype == "matrix") {
    if (length(dim(image)) == 3) {
      #Drop useless dimension of single channel array
      return(rayimg(
        process_image_preview(image[,, 1]),
        imagetype,
        gamma_correct
      ))
    } else {
      return(rayimg(process_image_preview(image), imagetype, gamma_correct))
    }
  } else if (imagetype == "png") {
    image = png::readPNG(image, ...)
    return(rayimg(
      process_image_preview(image),
      imagetype,
      gamma_correct
    ))
  } else if (imagetype == "tif") {
    image = tiff::readTIFF(image, ...)
    return(rayimg(process_image_preview(image), imagetype, gamma_correct))
  } else if (imagetype == "jpg") {
    image = jpeg::readJPEG(image, ...)
    return(rayimg(
      process_image_preview(image),
      imagetype,
      gamma_correct
    ))
  } else if (imagetype == "exr") {
    if (length(find.package("libopenexr", quiet = TRUE)) > 0) {
      image_tmp = libopenexr::read_exr(image)
      image = array(1, dim = c(image_tmp$height, image_tmp$width, 4))
      image[,, 1] = image_tmp$r
      image[,, 2] = image_tmp$g
      image[,, 3] = image_tmp$b
      return(rayimg(process_image_preview(image), imagetype, gamma_correct))
    } else {
      stop("The 'libopenexr' package is required for EXR support.")
    }
  } else {
    stop("This error should never be reached--please report a bug.")
  }
}
