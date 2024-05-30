#'@title Read Image
#'
#'@description Takes an RGB array/filename and adds an image overlay.
#'
#'@param image Image filename or 3-layer RGB array.
#'@param convert_to_array Default `TRUE`. Whether to convert 2D B&W images/matrices to RGBA arrays.
#'@param preview Default `FALSE`. If `TRUE`, it will display the image in addition
#'to returning it.
#'@param ... Arguments to pass to either `jpeg::readJPEG`, `png::readPNG`, or `tiff::readTIFF`.
#'
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
ray_read_image = function(image, convert_to_array = TRUE, preview = FALSE, ...) {
  process_image_dim = function(image2) {
    if(length(dim(image2)) == 2) {
      if(convert_to_array) {
        newarray = array(1,dim=c(nrow(image2),ncol(image2),3))
        newarray[,,1] = image2
        newarray[,,2] = image2
        newarray[,,3] = image2
      } else {
        return(image2)
      }
      if(preview) {
        plot_image(newarray)
      }
      return(newarray)
    } else if (dim(image2)[3] == 1) {
      if(convert_to_array) {
        newarray = array(1,dim=c(nrow(image2),ncol(image2),3))
        newarray[,,1] = image2[,,1]
        newarray[,,2] = image2[,,1]
        newarray[,,3] = image2[,,1]
      } else {
        return(image2)
      }
      if(preview) {
        plot_image(newarray)
      }
      return(newarray)
    } else if (dim(image2)[3] == 2) {
      newarray = array(1,dim=c(nrow(image2),ncol(image2),3))
      newarray[,,1] = image2[,,1]
      newarray[,,2] = image2[,,1]
      newarray[,,3] = image2[,,1]
      # newarray[,,4] = image2[,,2]
      if(preview) {
        plot_image(newarray)
      }
      return(newarray)
    } else if (dim(image2)[3] == 3) {
      newarray = array(1,dim=c(nrow(image2),ncol(image2),3))
      newarray[,,1] = image2[,,1]
      newarray[,,2] = image2[,,2]
      newarray[,,3] = image2[,,3]
      if(preview) {
        plot_image(newarray)
      }
      return(newarray)
    } else if (dim(image2)[3] == 4) {
      if(preview) {
        plot_image(image2)
      }
      return(image2)
    } else {
      stop("Unable to handle image of dimensions ", paste0(dim(image2), collapse = "x"))
    }
  }
  imagetype = get_file_type(image)
  if(imagetype == "array") {
    return(process_image_dim(image))
  } else if (imagetype == "matrix") {
    return(process_image_dim(image))
  } else if (imagetype == "png") {
    image = png::readPNG(image, ...)
    return(process_image_dim(image))
  } else if (imagetype == "tif") {
    image = tiff::readTIFF(image, ...)
    return(process_image_dim(image))
  } else if (imagetype == "jpg") {
    image = jpeg::readJPEG(image, ...)
    return(process_image_dim(image))
  } else {
    stop("This error should never be reached--please report a bug.")
  }
}
