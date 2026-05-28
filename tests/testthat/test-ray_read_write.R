test_that("Checking ray_read/ray_write", {
  rayfile_png = tempfile(fileext = ".png")
  clamped_dragon = render_clamp(dragon)
  rayimage::ray_write_image(filename = rayfile_png, image = clamped_dragon)
  dragon_png = rayimage::ray_read_image(image = rayfile_png)
  expect_true(compare_image(
    clamped_dragon,
    dragon_png,
    max_diff = 0.4,
    quantile_diff = 0.02
  ))

  rayfile_jpg = tempfile(fileext = ".jpg")
  rayimage::ray_write_image(
    filename = rayfile_jpg,
    image = clamped_dragon,
    quality = 1
  )
  dragon_jpg = rayimage::ray_read_image(
    image = rayfile_jpg,
    convert_to_array = TRUE
  )
  expect_true(compare_image(
    clamped_dragon,
    dragon_jpg,
    max_diff = 0.6,
    quantile_diff = 0.02
  ))

  rayfile_tiff = tempfile(fileext = ".tiff")
  expect_no_error(
    {
      rayimage::ray_write_image(
        filename = rayfile_tiff,
        image = rayimage::dragon
      )
    }
  )
  expect_warning(
    {
      dragon_tiff = rayimage::ray_read_image(image = rayfile_tiff)
    },
    regex = "Photometric"
  )
  expect_true(compare_image(
    clamped_dragon,
    dragon_tiff,
    quantile_diff = 0.02,
    max_diff = 0.5
  ))
})
