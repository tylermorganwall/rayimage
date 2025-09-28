#'@title Plot Image Grid
#'
#'@description Displays the image in the current device.
#'
#'@param input_list List of 3-layer RGB/4-layer RGBA array, `rayimg` class, or image filenames.
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
plot_image_grid = function(
  input_list,
  dim = c(1, 1),
  asp = 1,
  draw_grid = FALSE,
  gp = grid::gpar()
) {
  if (length(dim) != 2) stop("length of `dim` argument must equal 2")
  if (!inherits(input_list, "list"))
    stop("`input_list` must be a list of image arrays")

  if (length(asp) == 1) {
    asp = rep(asp, length(input_list))
  } else if (length(asp) != length(input_list)) {
    stop(sprintf(
      "length of `asp` (%i) not equal to length of `input_list` (%i) or length-1",
      length(asp),
      length(input_list)
    ))
  }

  if (length(input_list) > dim[1] * dim[2]) {
    warning(sprintf(
      "`input_list` of length %i, but grid only has %ix%i=%i slots: truncating list of images",
      length(input_list),
      dim[1],
      dim[2],
      dim[1] * dim[2]
    ))
    input_list = input_list[seq_len(dim[1] * dim[2])]
  }

  grid::grid.newpage()

  # optional background grid — push, draw, then pop
  if (draw_grid) {
    grid::pushViewport(
      grid::viewport(layout = grid::grid.layout(1, 1), name = "ray_bg_grid")
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
    grid::upViewport() # pop "ray_bg_grid"
  }

  # parent layout viewport
  grid::pushViewport(grid::viewport(
    name = "ray_image_grid",
    layout = grid::grid.layout(nrow = dim[1], ncol = dim[2]),
    gp = gp
  ))
  on.exit(
    {
      # ensure we fully unwind no matter what
      try(grid::upViewport(0), silent = TRUE)
    },
    add = TRUE
  )

  layout_idx = data.frame(
    y = rep(seq_len(dim[1]), each = dim[2]),
    x = rep(seq_len(dim[2]), dim[1])
  )

  for (i in seq_len(length(input_list))) {
    # enter the parent (in case caller changed current vp)
    grid::seekViewport("ray_image_grid")
    # push cell viewport, draw, then pop it
    grid::pushViewport(grid::viewport(
      layout.pos.row = layout_idx$y[i],
      layout.pos.col = layout_idx$x[i],
      name = sprintf("ray_cell_%i_%i", layout_idx$x[i], layout_idx$y[i]),
      gp = gp
    ))
    if (!is.null(input_list[[i]])) {
      rayimage::plot_image(input_list[[i]], asp = asp[i], new_page = FALSE)
    }
    grid::upViewport() # pop cell
  }

  grid::upViewport() # pop "ray_image_grid"
  invisible(NULL)
}
