#'@title Plot Image Grid
#'
#'@description Displays the image in the current device.
#'
#'@param input_list List of array (or matrix) image inputs.
#'@param dim Default `c(1,1)`. Width by height of output grid.
#'@param asp Default `1`. Aspect ratio of the pixels(s). For example, an aspect ratio of `4/3` will
#'slightly widen the image. This can also be a vector the same length of
#'`input_list` to specify an aspect ratio for each image in the grid.
#'@param draw_grid Default `FALSE`. If `TRUE`, this will draw a grid in the background to help
#'disambiguate the actual image from the device (helpful if the image background is the same as the
#'device's background).
#'@param gp A `grid::gpar()` object to include for the grid viewport displaying the image.
#'@export
#'@examples
#'if(run_documentation()){
#'#Plot the dragon array
#'plot_image_grid(list(dragon, 1-dragon), dim = c(1,2))
#'}
#'if(run_documentation()){
#'plot_image_grid(list(dragon, 1-dragon), dim = c(2,1))
#'}
#'if(run_documentation()){
#'plot_image_grid(list(dragon, NULL, 1-dragon), dim = c(2,2), asp = c(2,1,1/2))
#'}
#'if(run_documentation()){
#'plot_image_grid(list(dragon, NULL, NULL, dragon), dim = c(2,2), asp = c(2,1,1,1/2))
#'}
#'if(run_documentation()){
#'#Plot alongside the depth matrix
#'dragon_depth_reoriented = render_reorient(dragondepth,
#'                                          transpose = TRUE,
#'                                          flipx = TRUE)/2000
#'plot_image_grid(list(dragondepth/2000, dragon, dragon, dragondepth/2000),
#'                dim = c(2,2))
#'}
plot_image_grid = function(input_list, dim = c(1,1), asp = 1, draw_grid = FALSE,
                           gp = grid::gpar()) {
  if(length(dim) != 2) {
    stop("length of `dim` argument must equal 2")
  }
  if(length(asp) == 1) {
    asp = rep(asp, length(input_list))
  } else {
    if(length(asp) != length(input_list)) {
      stop(sprintf("length of `asp` (%i) not equal to length of `input_list` (%i) or length-1", length(asp), length(input_list)))
    }
  }
  if(!inherits(input_list, "list")) {
    stop("`input_list` must be a list of image arrays")
  }
  if(length(input_list) > dim[1]*dim[2]) {
    warning(sprintf("`input_list` of length %i, but grid only has %ix%i=%i slots: truncating list of images",
                    length(input_list), dim[1], dim[2], dim[1]*dim[2]))
    input_list = input_list[1:(dim[1]*dim[2])]
  }
  grid::grid.newpage()
  if(draw_grid) {
    draw_grid_fxn = function() {
      grid::pushViewport(
        grid::viewport(layout = grid::grid.layout(1, 1,
                                                  widths = grid::unit(1, "npc"),
                                                  heights = grid::unit(1, "npc")))
      )
      # Define grid density and angle
      grid_density = 0.01 # Adjust this value for tighter or looser grid
      for (i in seq(-2, 2, by = grid_density)) {
        grid::grid.lines(x = c(0, 1), y = c(i, i + 1), default.units = "npc", gp = grid::gpar(col = "grey"))
        grid::grid.lines(x = c(0, 1), y = c(i, i - 1), default.units = "npc", gp = grid::gpar(col = "grey"))
      }
    }
    draw_grid_fxn()
  }
  gl = grid::grid.layout(nrow = dim[1],
                         ncol = dim[2])
  layout_idx = data.frame(y=rep(seq_len(dim[1]),each=dim[2]),
                          x=rep(seq_len(dim[2]), dim[1]))
  grid::pushViewport(grid::viewport(name = "image_array",
                                    layout = gl, gp = gp))
  for(i in seq_len(length(input_list))) {
    grid::seekViewport("image_array")
    grid::pushViewport(viewport = grid::viewport(layout.pos.row = layout_idx$y[i],
                                                 layout.pos.col = layout_idx$x[i],
                                                 name = sprintf("grid_%i_%i",
                                                                layout_idx$x[i],
                                                                layout_idx$y[i]),
                                                 gp = gp))
    if(!is.null(input_list[[i]])) {
      rayimage::plot_image(input_list[[i]],
                           asp = asp[i],
                           new_page = FALSE)
    }
  }
}
