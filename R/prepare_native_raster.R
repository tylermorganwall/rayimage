prepare_native_raster = function(image, angle = 0, show_linear = FALSE) {
  img = ray_read_image(image, convert_to_array = TRUE)

  display = img
  cs_from = attr(display, "colorspace")
  white_curr = attr(display, "white_current")

  display = render_clamp(display)
  display[is.na(display)] = 0

  alpha_channel = unclass(render_clamp(
    display[,, 4],
    min_value = 0,
    max_value = 1
  ))
  display[,, 4] = alpha_channel

  if (!show_linear) {
    if (
      !is.null(cs_from) &&
        (!identical(cs_from$name, "sRGB") ||
          any(abs(white_curr - CS_SRGB$white_xyz) > 1e-8))
    ) {
      xyz = apply_color_matrix(display, cs_from$rgb_to_xyz)
      if (any(abs(white_curr - CS_SRGB$white_xyz) > 1e-8)) {
        CAT = compute_cat_bradford(white_curr, CS_SRGB$white_xyz)
        xyz = apply_color_matrix(xyz, CAT)
      }
      display = apply_color_matrix(xyz, CS_SRGB$xyz_to_rgb)
      display[,, 1:3][display[,, 1:3] < 0] = 0
    }
    display[,, 1:3] = to_srgb(display[,, 1:3])
  }

  if (!isTRUE(all.equal(angle %% 360, 0))) {
    display = rotate_image_array(display, angle)
  }

  list(
    native_raster = convert_to_native_raster(display),
    display_dim = dim(display)
  )
}
