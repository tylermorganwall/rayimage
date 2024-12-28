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
#'if(run_documentation()){
#'#Plot the dragon
#'plot_image(dragon)
#'}
#'if(run_documentation()){
#'#Add a vignette effect:
#'render_vignette(dragon, preview = TRUE, vignette = 0.5)
#'}
#'if(run_documentation()){
#'#Darken the vignette effect:
#'render_vignette(dragon, preview = TRUE, vignette = 1)
#'}
#'if(run_documentation()){
#'#Change the radius:
#'render_vignette(dragon, preview = TRUE, vignette = 1, radius=1.5)
#'render_vignette(dragon, preview = TRUE, vignette = 1, radius=0.5)
#'}
#'if(run_documentation()){
#'#Change the color:
#'render_vignette(dragon, preview = TRUE, vignette = 1, color="white")
#'}
#'if(run_documentation()){
#'#Increase the width of the blur by 50%:
#'render_vignette(dragon, preview = TRUE, vignette = c(1,1.5))
#'}
render_vignette = function(image, vignette = 0.5, color = "#000000", radius = 1.3,
                        filename = NULL, preview = FALSE) {
  temp = tempfile(fileext = ".png")
  ray_write_image(image, temp)

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
  handle_image_output(temp, filename = filename, preview = preview)
}

#'@title Add Vignette Effect (Deprecated)
#'
#'@description Takes an RGB array/filename and adds a camera vignette effect.
#'
#'@param ... Arguments to pass to `render_title()` function.

#'@return 3-layer RGB array of the processed image.
#'@import grDevices
#'@export
#'@examples
#'if(run_documentation()){
#'#Plot the dragon
#'plot_image(dragon)
#'}
#'if(run_documentation()){
#'#Add a vignette effect:
#'add_vignette(dragon, preview = TRUE, vignette = 0.5)
#'}
add_vignette = function(...) {
  message("add_vignette() deprecated--use render_vignette() instead.")
  render_vignette(...)
}

