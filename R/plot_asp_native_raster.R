#'@title Plot Native Raster with Custom Aspect Ratio
#'
#'@keywords internal
plot_asp_native_raster = function(nr, asp = 1, show = FALSE, return_grob = FALSE, gp = grid::gpar()) {
  stopifnot(asp > 0)
  native_dim = dim(nr)
  image_width = native_dim[2]
  image_height = native_dim[1]
  asp = asp * image_width / image_height
  if (asp > 1) {
      gl = grid::grid.layout(1,1,
                              widths = grid::unit(1, "null"),
                              heights = grid::unit(1 / asp, "null"), respect = TRUE)
  } else {
      gl = grid::grid.layout(1,1,
                              widths = grid::unit(1 * asp, "null"),
                              heights = grid::unit(1, "null"), respect = TRUE)
  }

  top.vp = grid::viewport(layout = gl,
                           name = "grid_asp_container")
  grid::pushViewport(top.vp)
  grid::pushViewport(grid::viewport(name = "image",
                                    layout.pos.col = 1,
                                    layout.pos.row = 1,
                                    xscale = c(0, image_width),
                                    yscale = c(image_height, 0),
                                  gp = gp))  # yscale reversed for top-left origin

  if (!return_grob) {
      grid::grid.raster(nr,
                        interpolate = FALSE,
                        width = grid::unit(1,"npc"),
                        height = grid::unit(1,"npc"),
                      gp = gp)
      return(invisible())
  } else {
      return(grid::rasterGrob(nr, interpolate = FALSE,
                              width = grid::unit(1,"npc"),
                              height = grid::unit(1,"npc")),
                              gp = gp)
  }
}
