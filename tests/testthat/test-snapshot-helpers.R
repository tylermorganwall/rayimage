test_that("compare_image normalizes RGB and RGBA PNG channel counts", {
  rgb = array(1, dim = c(8, 9, 3))

  rgba = array(1, dim = c(8, 9, 4))
  rgba[,, 4] = 1

  path_rgb = tempfile(fileext = ".png")
  path_rgba = tempfile(fileext = ".png")

  png::writePNG(rgb, target = path_rgb)
  png::writePNG(rgba, target = path_rgba)

  expect_true(compare_image(path_rgb, path_rgba, max_diff = 1e-8))
})

test_that("compare_image returns false rather than erroring on size mismatch", {
  img1 = array(1, dim = c(8, 9, 3))
  img2 = array(1, dim = c(9, 8, 3))

  path1 = tempfile(fileext = ".png")
  path2 = tempfile(fileext = ".png")

  png::writePNG(img1, target = path1)
  png::writePNG(img2, target = path2)

  expect_warning(expect_false(compare_image(path1, path2)), "Dim not the same")
})
