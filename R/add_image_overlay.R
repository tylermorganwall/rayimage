#'@title Add Overlay
#'
#'@description Takes an RGB array/filename and adds an image overlay.
#'
#'@param image Image filename or 3-layer RGB array.
#'@param image_overlay Default `NULL`. Either a string indicating the location of a png image to overlay
#'over the image (transparency included), or a 4-layer RGBA array. This image will be resized to the
#'dimension of the image if it does not match exactly.
#'@param rescale_original Default `FALSE`. If `TRUE`, function will resize the original image to match
#'the overlay.
#'@param alpha Default `NULL`, using overlay's alpha channel. Otherwise, this sets the alpha transparency
#'by multiplying the existing alpha channel by this value (between 0 and 1).
#'@param filename Default `NULL`. File to save the image to. If `NULL` and `preview = FALSE`,
#'returns an RGB array.
#'@param preview Default `FALSE`. If `TRUE`, it will display the image in addition
#'to returning it.

#'@return 3-layer RGB array of the processed image.
#'@import grDevices
#'@export
#'@examples
#'if(rayimage:::run_documentation()){
#'#Plot the dragon
#'plot_image(dragon)
#'}
#'if(rayimage:::run_documentation()){
#'#Add an overlay of a red semi-transparent circle:
#'circlemat = generate_2d_disk(min(dim(dragon)[1:2]))
#'circlemat = circlemat/max(circlemat)
#'
#'#Create RGBA image, with a transparency of 0.5
#'rgba_array = array(1, dim=c(nrow(circlemat),ncol(circlemat),4))
#'rgba_array[,,1] = circlemat
#'rgba_array[,,2] = 0
#'rgba_array[,,3] = 0
#'dragon_clipped = dragon
#'dragon_clipped[dragon_clipped > 1] = 1
#'add_image_overlay(dragon_clipped, image_overlay = rgba_array,
#'                  alpha=0.5, preview = TRUE)
#'}
add_image_overlay = function(image, image_overlay = NULL, rescale_original = FALSE,
                             alpha = NULL, filename = NULL, preview = FALSE) {
  imagetype = get_file_type(image)
  image_overlay_type = get_file_type(image)

  temp = tempfile(fileext = ".png")
  temp_overlay = tempfile(fileext = ".png")

  if(imagetype == "array") {
    #Clip HDR images
    image[image > 1] = 1
    png::writePNG(image,temp)
    image = temp
  } else if (imagetype == "matrix") {
    newarray = array(1,dim=c(nrow(image),ncol(image),4))
    newarray[,,1] = image
    newarray[,,2] = image
    newarray[,,3] = image
    #Clip HDR images
    newarray[newarray > 1] = 1
    png::writePNG(newarray,temp)
    image = temp
  } else if (imagetype == "png") {
    image = png::readPNG(image)
    png::writePNG(image,temp)
  } else if (imagetype == "jpg") {
    image = jpeg::readJPEG(image)
    png::writePNG(image,temp)
  }
  if(image_overlay_type == "array") {
    #Clip HDR images
    image_overlay[image_overlay > 1] = 1
    png::writePNG(image_overlay,temp_overlay)
    image_overlay = temp_overlay
  } else if (image_overlay_type == "matrix") {
    newarray = array(1,dim=c(nrow(image_overlay),ncol(image_overlay),4))
    newarray[,,1] = image_overlay
    newarray[,,2] = image_overlay
    newarray[,,3] = image_overlay
    #Clip HDR images
    newarray[newarray > 1] = 1
    png::writePNG(newarray,temp_overlay)
    image_overlay = temp_overlay
  } else if (image_overlay_type == "png") {
    image_overlay = png::readPNG(image_overlay)
    png::writePNG(image_overlay,temp_overlay)
  } else if (image_overlay_type == "jpg") {
    image_overlay = jpeg::readJPEG(image_overlay)
    png::writePNG(image_overlay,temp_overlay)
  }
  tempmap = aperm(png::readPNG(temp),c(2,1,3))

  dimensions = dim(tempmap)

  if(!("magick" %in% rownames(utils::installed.packages()))) {
    stop("`magick` package required for adding overlay")
  }
  if(is.null(image_overlay)) {
    stop("Need to pass in image to image_overlay argument.")
  }
  if(inherits(image_overlay,"character")) {
    tempover = png::readPNG(image_overlay)
    if(length(dim(tempover)) == 3 && dim(tempover)[3] == 4 && !is.null(alpha)) {
      if(alpha >= 0 && alpha <= 1) {
        tempover[,,4] = tempover[,,4] * alpha
      } else {
        stop("alpha needs to be between 0 and 1")
      }
    }
    if(length(dim(tempover)) == 3 && dim(tempover)[3] == 3 && !is.null(alpha)) {
      newarray = array(alpha,dim = dim(tempover) + c(0,0,1))
      if(alpha >= 0 && alpha <= 1) {
        newarray[,,1:3] = tempover
        tempover = newarray
      } else {
        stop("alpha needs to be between 0 and 1")
      }
    }
    if(length(dim(image_overlay)) == 2 && !is.null(alpha)) {
      newarray = array(alpha,dim = c(dim(image_overlay),4))
      if(alpha >= 0 && alpha <= 1) {
        newarray[,,1:3] = tempover
        image_overlay = newarray
      } else {
        stop("alpha needs to be between 0 and 1")
      }
    }
    image_overlay_file = tempfile(fileext = ".png")
    png::writePNG(tempover, image_overlay_file)
  } else if(inherits(image_overlay,"array") || inherits(image_overlay,"matrix")) {
    if(length(dim(image_overlay)) == 3 && dim(image_overlay)[3] == 4 && !is.null(alpha)) {
      if(alpha >= 0 && alpha <= 1) {
        image_overlay[,,4] = image_overlay[,,4] * alpha
      } else {
        stop("alpha needs to be between 0 and 1")
      }
    }
    if(length(dim(image_overlay)) == 3 && dim(image_overlay)[3] == 3 && !is.null(alpha)) {
      newarray = array(alpha,dim = dim(image_overlay) + c(0,0,1))
      if(alpha >= 0 && alpha <= 1) {
        newarray[,,1:3] = image_overlay
        image_overlay = newarray
      } else {
        stop("alpha needs to be between 0 and 1")
      }
    }
    if(length(dim(image_overlay)) == 2 && !is.null(alpha)) {
      newarray = array(alpha,dim = c(dim(image_overlay),4))
      if(alpha >= 0 && alpha <= 1) {
        newarray[,,1:3] = image_overlay
        image_overlay = newarray
      } else {
        stop("alpha needs to be between 0 and 1")
      }
    }
    image_overlay_file = tempfile()
    png::writePNG(image_overlay, image_overlay_file)
  }
  tempover = png::readPNG(image_overlay_file)
  if(length(dim(tempover)) == 3) {
    tempover = aperm(tempover, c(2,1,3))
  } else {
    temparray = array(1,dim=c(dim(tempover),4))
    temparray[,,1:3] = tempover
    tempover = aperm(temparray, c(2,1,3))
  }
  dimensions_overlay = dim(tempover)
  if(!rescale_original) {
    magick::image_read(temp) |>
      magick::image_composite(
        magick::image_scale(magick::image_read(image_overlay_file),
                            paste0(dimensions[1],"x",dimensions[2],"!")), operator = "Over"
      ) |>
      magick::image_write(path = temp, format = "png")
  } else {
    magick::image_read(temp) |>
      magick::image_scale(paste0(dimensions_overlay[1],"x",dimensions_overlay[2],"!")) |>
      magick::image_composite(magick::image_read(image_overlay_file), operator = "Over") |>
      magick::image_write(path = temp, format = "png")
  }
  temp = png::readPNG(temp)
  if(length(dim(temp)) == 3 && dim(temp)[3] == 2) {
    temparray = array(1,dim = c(nrow(temp),ncol(temp),4))
    temparray[,,1] = temp[,,1]
    temparray[,,2] = temp[,,1]
    temparray[,,3] = temp[,,1]
    temparray[,,4] = temp[,,2]
    temp = temparray
  }
  if(length(dim(temp)) == 2) {
    temparray = array(1,dim = c(nrow(temp),ncol(temp),4))
    temparray[,,1] = temp
    temparray[,,2] = temp
    temparray[,,3] = temp
    temp = temparray
  }
  if(missing(filename)) {
    if(!preview) {
      return(temp)
    }
    plot_image(temp)
    return(invisible(temp))
  } else {
    save_png(temp, filename)
    return(invisible(temp))
  }
}
