test_that("render_sprite_overlay preserves RGB output channels", {
  image = array(0, dim = c(4, 4, 3))
  overlay = array(0, dim = c(2, 2, 4))
  overlay[,, 1] = 1
  overlay[,, 4] = 0.5

  output = render_sprite_overlay(
    image,
    overlay,
    convert_overlay_colorspace = FALSE,
    overlay_coords = c(2, 2),
    preserve_channels = TRUE,
    preview = FALSE
  )

  expect_equal(dim(output), c(4, 4, 3))
  expect_equal(unclass(output)[2:3, 2:3, 1], matrix(0.5, 2, 2))
})

test_that("render_sprite_overlay preserves RGBA output alpha", {
  image = array(0, dim = c(3, 3, 4))
  image[,, 4] = 0.5
  overlay = array(0, dim = c(3, 3, 4))
  overlay[,, 1] = 1
  overlay[,, 4] = 0.5

  output = render_sprite_overlay(
    image,
    overlay,
    convert_overlay_colorspace = FALSE,
    preserve_channels = TRUE,
    preview = FALSE
  )

  expect_equal(dim(output), c(3, 3, 4))
  expect_equal(unclass(output)[,, 4], matrix(0.75, 3, 3))
})

test_that("render_sprite_overlay places overlays with numeric justification", {
  image = array(0, dim = c(6, 8, 3))
  overlay = array(0, dim = c(2, 3, 4))
  overlay[,, 1] = 1
  overlay[,, 4] = 1

  expect_overlay_region = function(hjust, expected_cols) {
    output = render_sprite_overlay(
      image,
      overlay,
      convert_overlay_colorspace = FALSE,
      overlay_coords = c(6.2, 3.1),
      hjust = hjust,
      vjust = 0,
      preserve_channels = TRUE,
      preview = FALSE
    )
    expected = matrix(FALSE, nrow = 6, ncol = 8)
    expected[3:4, expected_cols] = TRUE
    expect_identical(unclass(output)[,, 1] > 0.9, expected)
  }

  expect_overlay_region(0, 6:8)
  expect_overlay_region(0.5, 5:7)
  expect_overlay_region(1, 3:5)

  output = render_sprite_overlay(
    image,
    overlay,
    convert_overlay_colorspace = FALSE,
    overlay_coords = c(6.2, 3.1),
    overlay_just = c(1, 0),
    preserve_channels = TRUE,
    preview = FALSE
  )
  expected = matrix(FALSE, nrow = 6, ncol = 8)
  expected[3:4, 3:5] = TRUE
  expect_identical(unclass(output)[,, 1] > 0.9, expected)
})

test_that("render_sprite_overlay crops overlays outside each image edge", {
  image = array(0, dim = c(4, 4, 3))
  overlay = array(0, dim = c(3, 3, 4))
  overlay[,, 1] = 1
  overlay[,, 4] = 1

  expect_cropped_region = function(coords, rows, cols) {
    output = render_sprite_overlay(
      image,
      overlay,
      convert_overlay_colorspace = FALSE,
      overlay_coords = coords,
      hjust = 0,
      vjust = 0,
      preserve_channels = TRUE,
      preview = FALSE
    )
    expected = matrix(FALSE, nrow = 4, ncol = 4)
    expected[rows, cols] = TRUE
    expect_identical(unclass(output)[,, 1] > 0.9, expected)
  }

  expect_cropped_region(c(0, 2), 2:4, 1:2)
  expect_cropped_region(c(3, 2), 2:4, 3:4)
  expect_cropped_region(c(2, 0), 1:2, 2:4)
  expect_cropped_region(c(2, 3), 3:4, 2:4)
})

test_that("render_padding adds transparent padding without edge replication", {
  image = array(0, dim = c(2, 3, 4))
  image[,, 1] = matrix(1:6, nrow = 2) / 6
  image[,, 2] = 0.25
  image[,, 3] = 0.5
  image[,, 4] = 1

  padded = render_padding(image, pad = 1, preview = FALSE)
  padded_array = unclass(padded)

  expect_equal(dim(padded), c(4, 5, 4))
  expect_equal(padded_array[2:3, 2:4, ], image)
  expect_true(all(padded_array[c(1, 4), , 4] == 0))
  expect_true(all(padded_array[, c(1, 5), 4] == 0))
  expect_true(all(padded_array[1, , 1:3] == 0))
  expect_true(all(padded_array[, 1, 1:3] == 0))

  asymmetric = render_padding(
    image,
    pad = c(1, 2, 3, 4),
    preview = FALSE
  )
  expect_equal(dim(asymmetric), c(6, 9, 4))
  expect_equal(unclass(asymmetric)[2:3, 5:7, ], image)

  opaque_color = render_padding(
    image,
    pad = 1,
    color = "red",
    preview = FALSE
  )
  opaque_array = unclass(opaque_color)
  expect_true(all(opaque_array[c(1, 4), , 1] == 1))
  expect_true(all(opaque_array[c(1, 4), , 2:3] == 0))
  expect_true(all(opaque_array[c(1, 4), , 4] == 1))

  transparent_color = render_padding(
    image,
    pad = 1,
    color = "red",
    alpha = 0.25,
    preview = FALSE
  )
  transparent_array = unclass(transparent_color)
  expect_true(all(transparent_array[c(1, 4), , 1] == 1))
  expect_true(all(transparent_array[c(1, 4), , 4] == 0.25))

  scalar_color = render_padding(
    image,
    pad = 1,
    color = 0.25,
    alpha = 0.5,
    preview = FALSE
  )
  scalar_array = unclass(scalar_color)
  expect_true(all(scalar_array[c(1, 4), , 1:3] == 0.25))
  expect_true(all(scalar_array[c(1, 4), , 4] == 0.5))

  rgba_color = render_padding(
    image,
    pad = 1,
    color = c(1, 0, 0, 0.25),
    preview = FALSE
  )
  rgba_array = unclass(rgba_color)
  expect_true(all(rgba_array[c(1, 4), , 1] == 1))
  expect_true(all(rgba_array[c(1, 4), , 2:3] == 0))
  expect_true(all(rgba_array[c(1, 4), , 4] == 0.25))

  rgba_override = render_padding(
    image,
    pad = 1,
    color = c(1, 0, 0, 0.25),
    alpha = 0.5,
    preview = FALSE
  )
  rgba_override_array = unclass(rgba_override)
  expect_true(all(rgba_override_array[c(1, 4), , 1] == 1))
  expect_true(all(rgba_override_array[c(1, 4), , 2:3] == 0))
  expect_true(all(rgba_override_array[c(1, 4), , 4] == 0.5))

  hex_alpha_color = render_padding(
    image,
    pad = 1,
    color = "#FF000040",
    preview = FALSE
  )
  hex_alpha_array = unclass(hex_alpha_color)
  expect_true(all(hex_alpha_array[c(1, 4), , 1] == 1))
  expect_true(all(hex_alpha_array[c(1, 4), , 2:3] == 0))
  expect_equal(hex_alpha_array[c(1, 4), , 4], array(64 / 255, dim = c(2, 5)))

  explicit_alpha = render_padding(
    image,
    pad = 1,
    color = "#FF000040",
    alpha = 0.5,
    preview = FALSE
  )
  explicit_alpha_array = unclass(explicit_alpha)
  expect_true(all(explicit_alpha_array[c(1, 4), , 1] == 1))
  expect_true(all(explicit_alpha_array[c(1, 4), , 2:3] == 0))
  expect_true(all(explicit_alpha_array[c(1, 4), , 4] == 0.5))

  expect_equal(
    convert_color(c(1, 0, 0, 0.25), as_hex = TRUE),
    "#FF000040"
  )

  expect_error(
    render_padding(image, pad = 1, color = c(1, 0), preview = FALSE),
    "invalid color"
  )
})

test_that("render_stack uses shared color parsing for backgrounds", {
  image = array(1, dim = c(1, 1, 4))
  image[,, 4] = 1
  wider = array(1, dim = c(1, 3, 4))
  wider[,, 4] = 1

  stacked = render_stack(
    list(image, wider),
    stack = "v",
    align = "left",
    background = c(0.1, 0.2, 0.3, 0.4),
    convert_colorspace = FALSE,
    preview = FALSE
  )
  stacked_array = unclass(stacked)
  expect_equal(stacked_array[1, 2:3, 1], c(0.1, 0.1))
  expect_equal(stacked_array[1, 2:3, 2], c(0.2, 0.2))
  expect_equal(stacked_array[1, 2:3, 3], c(0.3, 0.3))
  expect_equal(stacked_array[1, 2:3, 4], c(0.4, 0.4))

  transparent = render_stack(
    list(image, wider),
    stack = "v",
    align = "left",
    background = "transparent",
    convert_colorspace = FALSE,
    preview = FALSE
  )
  transparent_array = unclass(transparent)
  expect_true(all(transparent_array[1, 2:3, 4] == 0))

  expect_error(
    render_stack(
      list(image, wider),
      stack = "v",
      background = c(0.1, 0.2),
      convert_colorspace = FALSE,
      preview = FALSE
    ),
    "invalid color"
  )
})

test_that("render_alpha_outline builds padded alpha halos", {
  mask = matrix(c(0, 1, 1, 0), nrow = 2)

  unpadded = render_alpha_outline(
    mask = mask,
    expand = 0,
    color = "red",
    alpha = 0.4,
    preview = FALSE
  )
  expect_equal(dim(unpadded), c(2, 2, 4))
  expect_equal(unclass(unpadded)[,, 4], mask * 0.4)

  halo = render_alpha_outline(
    mask = mask,
    expand = 0,
    color = "red",
    alpha = 0.4,
    pad = 1,
    preview = FALSE
  )
  halo_array = unclass(halo)

  expect_equal(dim(halo), c(4, 4, 4))
  expect_equal(unname(attr(halo, "padding")), rep(1L, 4))
  expect_equal(halo_array[2:3, 2:3, 4], mask * 0.4)
  expect_true(all(halo_array[c(1, 4), , 4] == 0))
  expect_true(all(halo_array[, c(1, 4), 4] == 0))
})

test_that("render_image_overlay default compositing math is unchanged", {
  image = array(0.2, dim = c(2, 2, 3))
  overlay = array(0, dim = c(2, 2, 4))
  overlay[,, 1] = 0.8
  overlay[,, 2] = 0.4
  overlay[,, 3] = 0.1
  overlay[,, 4] = matrix(c(0.25, 0.5, 0.75, 1), nrow = 2)

  default_output = render_image_overlay(
    image,
    overlay,
    convert_overlay_colorspace = FALSE,
    preview = FALSE
  )
  sprite_output = render_sprite_overlay(
    image,
    overlay,
    convert_overlay_colorspace = FALSE,
    overlay_coords = c(1, 1),
    preserve_channels = FALSE,
    preview = FALSE
  )

  expected = array(1, dim = c(2, 2, 4))
  for (channel in 1:3) {
    expected[,, channel] = overlay[,, channel] *
      overlay[,, 4] +
      image[,, channel] * (1 - overlay[,, 4])
  }
  default_array = array(unclass(default_output), dim = dim(default_output))
  sprite_array = array(unclass(sprite_output), dim = dim(sprite_output))

  expect_equal(default_array, expected)
  expect_equal(default_array, sprite_array)
})

test_that("render_image_overlay still resizes mismatched overlays by default", {
  image = array(0, dim = c(4, 5, 3))
  overlay = array(0, dim = c(2, 2, 4))
  overlay[,, 1] = 1
  overlay[,, 4] = 1

  output = render_image_overlay(
    image,
    overlay,
    convert_overlay_colorspace = FALSE,
    preview = FALSE
  )

  expect_equal(dim(output), c(4, 5, 4))
  expect_equal(
    unclass(output)[,, 1],
    matrix(1, nrow = 4, ncol = 5),
    tolerance = 1e-6
  )
})

test_that("render_image_overlay does not expose sprite placement arguments", {
  image = array(0, dim = c(2, 2, 3))
  overlay = array(0, dim = c(2, 2, 4))

  expect_error(
    render_image_overlay(
      image,
      overlay,
      overlay_coords = c(1, 1),
      preview = FALSE
    ),
    "unused argument"
  )
})
