#'@title Clamp Image
#'
#'@description Clamps an image to a user-specified range
#'
#'@param image 3-layer RGB/4-layer RGBA array, `rayimg` class, or filename of an image.
#'@param min_value Default `0`. Minimum value to clamp the image to.
#'@param max_value Default `1`. Maximum value to clamp the image to.
#'@param preview Default `FALSE`. If `TRUE`, it will display the image in addition
#'to returning it.
#'@param ... Arguments to pass to either `jpeg::readJPEG`, `png::readPNG`, or `tiff::readTIFF`.
#'
#'@return 3-layer RGB array of the processed image.
#'@import grDevices
#'@export
#'@examples
#'if(run_documentation()){
#'#The rnage of the unchanged image
#'range(dragon)
#'}
#'if(run_documentation()){
#'#Clamp the maximum and minimum values to one and zero
#'render_clamp(dragon) |>
#'  range()
#'}
render_clamp = function(
  image,
  min_value = 0,
  max_value = 1,
  preview = FALSE,
  ...
) {
  image = ray_read_image(image) #Always output RGBA array
  #Check if file or image before below:
  image[image < min_value] = min_value
  image[image > max_value] = max_value
  if (preview) {
    plot_image(image, ...)
  }
  return(image)
}
