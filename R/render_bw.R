#'@title Render Black and White
#'
#'@description Transforms an image to black and white, preserving luminance.
#'
#'@param image 3-layer RGB/4-layer RGBA array, `rayimg` class, or filename of an image.
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
render_bw = function(
  image,
  rgb_coef = c(0.2126, 0.7152, 0.0722),
  filename = NULL,
  preview = FALSE
) {
  stopifnot(length(rgb_coef) == 3 && is.numeric(rgb_coef))
  src = ray_read_image(image)
  d = dim(src)
  # luminance
  lum = rgb_coef[1] *
    src[,, 1] +
    rgb_coef[2] * src[,, 2] +
    rgb_coef[3] * src[,, 3]

  out = array(1, dim = c(d[1], d[2], 4))
  out[,, 1] = lum
  out[,, 2] = lum
  out[,, 3] = lum
  out[,, 4] = if (d[3] >= 4L) src[,, 4] else 1

  # keep filetype, wrap as rayimg, mark grey
  attr(out, "filetype") = attr(src, "filetype")
  class(out) = unique(c("rayimg", setdiff(class(out), "rayimg"), "array"))
  out = rayimg_mark_grey(out)

  handle_image_output(out, filename = filename, preview = preview)
}
