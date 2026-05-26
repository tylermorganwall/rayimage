test_that("ray_write_image writes EXR when libopenexr is available", {
  skip_on_cran()
  skip_if_not_installed("libopenexr")

  rgb = array(0.25, dim = c(2, 2, 3))
  rgb[,, 1] = 0.1
  rgb[,, 2] = 0.4
  rgb[,, 3] = 0.8

  rgba = array(0.5, dim = c(2, 2, 4))
  rgba[,, 4] = matrix(c(0, 0.25, 0.75, 1), nrow = 2)

  for (image in list(rgb, rgba)) {
    tmp = tempfile(fileext = ".exr")
    on.exit(unlink(tmp), add = TRUE)

    expect_no_error(ray_write_image(image, tmp))
    expect_true(file.exists(tmp))
    expect_gt(file.info(tmp)$size, 0)
  }
})

test_that("ray_write_image errors clearly when libopenexr is unavailable", {
  skip_on_cran()
  testthat::local_mocked_bindings(
    .libopenexr_available = function() FALSE,
    .package = "rayimage"
  )

  tmp = tempfile(fileext = ".exr")
  expect_error(
    ray_write_image(array(0.5, dim = c(2, 2, 3)), tmp),
    "required to write EXR files"
  )
  expect_false(file.exists(tmp))
})
