#'@title Plot Native Raster with Custom Aspect Ratio
#'
#'@keywords internal
plot_asp_native_raster = function(
  nr,
  asp = 1,
  show = FALSE,
  return_grob = FALSE,
  gp = grid::gpar(),
  angle = 0
) {
  stopifnot(asp > 0)
  native_dim = dim(nr)
  image_width = native_dim[2]
  image_height = native_dim[1]
  asp = asp * image_width / image_height
  if (asp > 1) {
    gl = grid::grid.layout(
      1,
      1,
      widths = grid::unit(1, "null"),
      heights = grid::unit(1 / asp, "null"),
      respect = TRUE
    )
  } else {
    gl = grid::grid.layout(
      1,
      1,
      widths = grid::unit(1 * asp, "null"),
      heights = grid::unit(1, "null"),
      respect = TRUE
    )
  }

  needs_rotation = !isTRUE(all.equal(angle %% 360, 0))
  clip_setting = if (needs_rotation) "off" else "inherit"

  top.vp = grid::viewport(
    layout = gl,
    name = "grid_asp_container",
    clip = clip_setting
  )
  grid::pushViewport(top.vp)
  grid::pushViewport(grid::viewport(
    name = "image",
    layout.pos.col = 1,
    layout.pos.row = 1,
    xscale = c(0, image_width),
    yscale = c(image_height, 0),
    gp = gp,
    clip = clip_setting
  )) # yscale reversed for top-left origin

  image_grob = grid::rasterGrob(
    nr,
    interpolate = FALSE,
    width = grid::unit(1, "npc"),
    height = grid::unit(1, "npc"),
    gp = gp
  )
  if (needs_rotation) {
    angle_norm = abs(angle) %% 180
    if (angle_norm > 90) {
      angle_norm = 180 - angle_norm
    }
    angle_rad = angle_norm * pi / 180
    width_rot = image_width * cos(angle_rad) + image_height * sin(angle_rad)
    height_rot = image_width * sin(angle_rad) + image_height * cos(angle_rad)
    scale_factor = max(width_rot / image_width, height_rot / image_height)
    if (!is.finite(scale_factor) || scale_factor <= 0) {
      scale_factor = 1
    }
    image_grob = grid::editGrob(
      image_grob,
      vp = grid::viewport(
        angle = angle,
        clip = "off",
        width = grid::unit(1 / scale_factor, "npc"),
        height = grid::unit(1 / scale_factor, "npc")
      )
    )
  }

  if (!return_grob) {
    grid::grid.draw(image_grob)
    return(invisible())
  } else {
    return(image_grob)
  }
}
