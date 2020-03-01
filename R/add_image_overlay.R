#'@title Add Overlay
#'
#'@description Takes an RGB array/filename and adds an image overlay.
#'
#'@param image Image filename or 3-layer RGB array.
#'@param image_overlay Default `NULL`. Either a string indicating the location of a png image to overlay
#'over the image (transparency included), or a 4-layer RGBA array. This image will be resized to the
#'dimension of the image if it does not match exactly.
#'@param filename Default `NULL`. File to save the image to. If `NULL` and `preview = FALSE`,
#'returns an RGB array.
#'@param preview Default `FALSE`. If `TRUE`, it will display the image in addition
#'to returning it.

#'@return 3-layer RGB array of the processed image.
#'@import grDevices
#'@export
#'@examples
#'#Plot the dragon
#'plot_image(dragon)
#'
#'#Add an overlay of a red semi-transparent circle:
#'circlemat = generate_2d_disk(min(dim(dragon)[1:2]))
#'circlemat = circlemat/max(circlemat)
#'
#'#Create RGBA image, with a transparency of 0.5
#'rgba_array = array(0, dim=c(nrow(circlemat),ncol(circlemat),4))
#'rgba_array[,,1] = circlemat
#'rgba_array[,,4] = 0.5
#'dragon_clipped = dragon
#'dragon_clipped[dragon_clipped > 1] = 1
#'\donttest{
#'add_image_overlay(dragon_clipped, image_overlay = rgba_array, preview = TRUE)
#'}
add_image_overlay = function(image, image_overlay = NULL, filename = NULL,  preview = FALSE) {
  imagetype = get_file_type(image)
  temp = tempfile(fileext = ".png")
  if(imagetype == "array") {
    #Clip HDR images
    image[image > 1] = 1
    png::writePNG(image,temp)
    image = temp
  } else if (imagetype == "matrix") {
    newarray = array(0,dim=c(nrow(image),ncol(image),3))
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
  tempmap = aperm(png::readPNG(temp),c(2,1,3))
  dimensions = dim(tempmap)

  if(!("magick" %in% rownames(utils::installed.packages()))) {
    stop("`magick` package required for adding overlay")
  }
  if(is.null(image_overlay)) {
    stop("Need to pass in image to image_overlay argument.")
  }
  if(inherits(image_overlay,"character")) {
    image_overlay_file = image_overlay
  } else if(inherits(image_overlay,"array")) {
    image_overlay_file = tempfile()
    png::writePNG(image_overlay, image_overlay_file)
  }

  magick::image_read(temp) %>%
    magick::image_composite(
      magick::image_scale(magick::image_read(image_overlay_file),
                          paste0(dimensions[1],"x",dimensions[2],"!"))
    ) %>%
    magick::image_write(path = temp, format = "png")
  temp = png::readPNG(temp)
  if(length(dim(temp)) == 3 && dim(temp)[3] == 2) {
    temparray = array(0,dim = c(nrow(temp),ncol(temp),3))
    temparray[,,1] = temp[,,1]
    temparray[,,2] = temp[,,1]
    temparray[,,3] = temp[,,1]
    temp = temparray
  }
  if(length(dim(temp)) == 2) {
    temparray = array(0,dim = c(nrow(temp),ncol(temp),3))
    temparray[,,1] = temp
    temparray[,,2] = temp
    temparray[,,3] = temp
    temp = temparray
  }
  if(missing(filename)) {
    if(!preview) {
      return(temp)
    }
    plot_image(temp, keep_user_par = FALSE)
    return(invisible(temp))
  } else {
    save_png(temp, filename)
    return(invisible(temp))
  }
}
