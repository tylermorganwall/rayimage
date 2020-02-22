#'@title Add Vignette Effect
#'
#'@description Takes an RGB array/filename and adds a camera vignette effect.
#'
#'@param image Image filename or 3-layer RGB array.
#'@param filename Default `NULL`. File to save the image to. If `NULL` and `preview = FALSE`,
#'returns an RGB array.
#'@param vignette Default `0.5`. A camera vignetting effect will be added to the image.
#'`1` is the darkest vignetting, while `0` is no vignetting. If vignette is a length-2 vector, the second entry will
#'control the blurriness of the vignette effect (`1` is the default, e.g. `2` would double the blurriness but would take
#'much longer to compute).
#'@param preview Default `FALSE`. If `TRUE`, it will display the image in addition
#'to returning it.

#'@return 3-layer RGB array of the processed image.
#'@import grDevices
#'@export
#'@examples
#'#Plot the dragon
#'plot_image(dragon)
#'
#'#Add a vignette effect:
#'\donttest{
#'add_vignette(dragon, preview = TRUE, vignette = 0.5)
#'}
#'#Darken the vignette effect:
#'\donttest{
#'add_vignette(dragon, preview = TRUE, vignette = 1)
#'}
#'
#'#Increase the width of the blur by 50%:
#'\donttest{
#'add_vignette(dragon, preview = TRUE, vignette = c(1,1.5))
#'}
add_vignette = function(image, vignette = 0.5, filename = NULL, preview = FALSE) {
  imagetype = get_file_type(image)
  if(imagetype == "array") {
    temp = tempfile(fileext = ".png")
    #Clip HDR images
    image[image > 1] = 1
    png::writePNG(image,temp)
    image = temp
  }
  tempmap = png::readPNG(temp)
  dimensions = dim(tempmap)

  if(!("magick" %in% rownames(utils::installed.packages()))) {
    stop("`magick` package required for adding overlay")
  }
  if(length(vignette) > 1) {
    if(vignette[2] < 0) {
      stop("vignette[2] must be greater than 0")
    }
    radiusval = min(c(dimensions[1],dimensions[2]))/2 * vignette[2]
    vignette = vignette[1]
  } else {
    radiusval = min(c(dimensions[1],dimensions[2]))/2
  }
  if(is.numeric(vignette)) {
    if(vignette[1] > 1 || vignette[1] < 0) {
      stop("vignette value (", vignette[1],") must be between 0 and 1.")
    }
  } else {
    vignette = 0.4
  }
  imagefile = make_vignette_overlay(dimensions[1],dimensions[2], vignette, radiusval)
  magick::image_read(temp) %>%
    magick::image_composite(magick::image_read(imagefile)) %>%
    magick::image_write(path = temp, format = "png")
  temp = png::readPNG(image)
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
