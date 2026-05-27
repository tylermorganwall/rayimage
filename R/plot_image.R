#'@title Plot Image
#'
#'@description Displays the image in the current device.
#'
#'@param image 3-layer RGB/4-layer RGBA array, `rayimg` class, or filename of an image.
#'@param draw_grid Default `FALSE`. If `TRUE`, this will draw a grid in the background to help
#'disambiguate the actual image from the device (helpful if the image background is the same as the
#'device's background).
#'@param asp Default `1`. Aspect ratio of the pixels in the plot. For example, an aspect ratio of `4/3` will
#'slightly widen the image.
#'@param new_page  Default `TRUE`. Whether to call `grid::grid.newpage()` before plotting the image.
#'@param ignore_alpha Default `FALSE`. Whether to ignore the alpha channel when plotting.
#'@param return_grob Default `FALSE`. Whether to return the grob object.
#'@param gp A `grid::gpar()` object to include for the grid viewport displaying the image.
#'@param angle Default `0`. Counter-clockwise rotation (in degrees) applied before plotting.
#'@param show_linear Default `FALSE`. Most data should be gamma corrected before displaying on a screen.
#' Set to `TRUE` to turn off this gamma correction.
#'@export
#'@examples
#'#if(interactive()){
#'#Plot the dragon array
#'plot_image(dragon)
#'#Make pixels twice as wide as tall
#'plot_image(dragon, asp = 2)
#'#Plot non-square images
#'plot_image(dragon[1:100,,])
#'#Make pixels twice as tall as wide
#'plot_image(dragon[1:100,,], asp = 1/2)
#'#end}
plot_image = function(
  image,
  draw_grid = FALSE,
  ignore_alpha = FALSE,
  asp = 1,
  new_page = TRUE,
  return_grob = FALSE,
  gp = grid::gpar(),
  angle = 0,
  show_linear = FALSE
) {
  prepared = prepare_native_raster(
    image,
    angle = angle,
    show_linear = show_linear
  )
  nr = prepared$native_raster

  if (new_page) {
    grid::grid.newpage()
  }
  image_dim = prepared$display_dim

  if (draw_grid) {
    draw_grid_fxn = function() {
      grid::pushViewport(
        grid::viewport(
          layout = grid::grid.layout(
            1,
            1,
            widths = grid::unit(image_dim[1], "pt"),
            heights = grid::unit(image_dim[2], "pt")
          ),
          gp = gp
        )
      )
      grid_density = 0.01
      for (i in seq(-2, 2, by = grid_density)) {
        grid::grid.lines(
          x = c(0, 1),
          y = c(i, i + 1),
          default.units = "npc",
          gp = grid::gpar(col = "grey")
        )
        grid::grid.lines(
          x = c(0, 1),
          y = c(i, i - 1),
          default.units = "npc",
          gp = grid::gpar(col = "grey")
        )
      }
      grid::popViewport()
    }
    draw_grid_fxn()
  }
  return(plot_asp_native_raster(
    nr,
    asp = asp,
    return_grob = return_grob,
    gp = gp
  ))
}
