#'@title Add Vignette Effect
#'
#'@description Takes an RGB array/filename and adds a camera vignette effect.
#'
#'@param image Image filename or 3-layer RGB array.
#'@param filename Default `NULL`. Filename which to save the image. If `NULL` and `preview = FALSE`,
#'returns an RGB array.
#'@param vignette Default `0.5`. A camera vignetting effect will be added to the image.
#'`1` is the darkest vignetting, while `0` is no vignetting. If vignette is a length-2 vector, the second entry will
#'control the blurriness of the vignette effect (`1` is the default, e.g. `2` would double the blurriness but would take
#'much longer to compute).
#'@param radius Default `1.3`. Multiplier for the size of the vignette. If `1`, the vignette touches
#'the edge of the image.
#'@param color Default `"#000000"` (black). Color of the vignette.
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
#'#Add a vignette effect:
#'add_vignette(dragon, preview = TRUE, vignette = 0.5)
#'}
#'if(rayimage:::run_documentation()){
#'#Darken the vignette effect:
#'add_vignette(dragon, preview = TRUE, vignette = 1)
#'}
#'if(rayimage:::run_documentation()){
#'#Change the radius:
#'add_vignette(dragon, preview = TRUE, vignette = 1, radius=1.5)
#'add_vignette(dragon, preview = TRUE, vignette = 1, radius=0.5)
#'}
#'if(rayimage:::run_documentation()){
#'#Change the color:
#'add_vignette(dragon, preview = TRUE, vignette = 1, color="white")
#'}
#'if(rayimage:::run_documentation()){
#'#Increase the width of the blur by 50%:
#'add_vignette(dragon, preview = TRUE, vignette = c(1,1.5))
#'}
add_vignette = function(image, vignette = 0.5, color = "#000000", radius = 1.3,
                        filename = NULL, preview = FALSE) {
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
  imagefile = make_vignette_overlay(width=dimensions[1],height=dimensions[2],
                                    intensity=vignette, radius=radiusval, radius_multiplier = radius, color=color)
  magick::image_read(temp) |>
    magick::image_composite(magick::image_read(imagefile)) |>
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
