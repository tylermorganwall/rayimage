#'@title Render Black and White
#'
#'@description Transforms an image to black and white, preserving luminance.
#'
#'@param image Image filename, 3-layer RGB array, or matrix.
#'@param rgb_coef Default `c(0.2126, 0.7152, 0.0722)`.
#'Length-3 numeric vector listing coefficients to convert RGB to luminance.
#'@param filename Default `NULL`. The filename of the image to be saved. If this is not given, the image will be plotted instead.
#'@param preview Default `FALSE`. Whether to plot the convolved image, or just to return the values.
#'@return 3-layer RGB resized array or matrix.
#'@export
#'@examples
#'if(run_documentation()){
#'#Plot the image with a title
#'dragon |>
#'  render_title("Dragon", title_offset=c(10,10), title_bar_color="black",
#'            title_size=20, title_color = "white") |>
#'  render_bw(preview = TRUE)
#'}
render_bw = function(image, rgb_coef = c(0.2126, 0.7152, 0.0722),
                     filename=NULL, preview=FALSE) {
  stopifnot(length(rgb_coef) == 3 && is.numeric(rgb_coef))
  temp_image = ray_read_image(image)

  # Calculate luminance
  temp_image = rgb_coef[1] * temp_image[,,1] + rgb_coef[2] * temp_image[,,2] + rgb_coef[3] * temp_image[,,3]

  handle_image_output(temp_image, filename = filename, preview = preview)
}
