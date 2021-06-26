#'@title Reorient Image
#'
#'@description Reorients an image or matrix. Transformations are applied in this order: x, y, and transpose.
#'
#'@param image Image filename, 3-layer RGB array, or matrix.
#'@param flipx Default `FALSE`. Flip horizontally
#'@param flipy Default `FALSE`. Flip vertically.
#'@param transpose Default `FALSE`. Transpose image.
#'@param filename Default `NULL`. The filename of the image to be saved. If this is not given, the image will be plotted instead.
#'@param preview Default `FALSE`. Whether to plot the convolved image, or just to return the values.
#'@return 3-layer RGB reoriented array or matrix.
#'@export
#'@examples
#'#if(interactive()){
#'#Original orientation
#'\donttest{
#'plot_image(dragon)
#'}
#'
#'#Flip the dragon image horizontally
#'\donttest{
#'dragon %>%
#'  render_reorient(flipx = TRUE) %>%
#'  plot_image()
#'}
#'
#'#Flip the dragon image vertically
#'\donttest{
#'dragon %>%
#'  render_reorient(flipy = TRUE) %>%
#'  plot_image()
#'}
#'
#'#'#Transpose the dragon image
#'\donttest{
#'dragon %>%
#'  render_reorient(transpose = TRUE) %>%
#'  plot_image()
#'}
#'#end}
render_reorient = function(image, flipx = FALSE, flipy = FALSE, transpose = FALSE,
                           filename=NULL, preview=FALSE) {
  if(!is.null(filename)) {
    if(tools::file_ext(filename) != "png") {
      filename = paste0(filename,".png")
    }
  }
  imagetype = get_file_type(image)
  if(imagetype == "array") {
    temp_image = image
  } else if(imagetype == "jpg") {
    temp_image = suppressWarnings(jpeg::readJPEG(image))
  } else if (imagetype == "png"){
    temp_image = suppressWarnings(png::readPNG(image))
  } else if (imagetype == "matrix") {
    temp_image = image
  }
  if(flipx) {
    temp_image = fliplr(temp_image)
  }
  if(flipy) {
    temp_image = flipud(temp_image)
  }
  if(transpose) {
    if (imagetype == "matrix") {
      temp_image = t(temp_image)
    } else {
      temp_image = aperm(temp_image,c(2,1,3))
    }
  }
  if(is.null(filename)) {
    if(preview) {
      temp_image[temp_image > 1] = 1
      temp_image[temp_image < 0] = 0
      plot_image(temp_image)
    } else {
      temp_image
    }
  } else {
    temp_image[temp_image > 1] = 1
    temp_image[temp_image < 0] = 0
    save_png(temp_image, filename)
  }
}
