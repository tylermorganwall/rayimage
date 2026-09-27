test_that("HBD returns linear pixels and applies display encoding only once", {
  values = c(0, 0.001, 0.01, 0.18, 0.5, 1, 4, 16)
  pixels = array(1, c(2, 4, 4))
  pixels[,, 1:3] = array(rep(values, 3), c(2, 4, 3))
  image = ray_read_image(
    pixels,
    assume_colorspace = CS_SRGB,
    assume_white = "D65"
  )
  image[,, 4] = matrix(seq(0.1, 0.8, length.out = 8), 2, 4)
  x = pmax(values - 0.004, 0)
  encoded = (x * (6.2 * x + 0.5)) / (x * (6.2 * x + 1.7) + 0.06)

  mapped = render_tonemap(image, method = "hbd")
  expect_true(attr(mapped, "source_linear"))
  expect_equal(as.numeric(mapped[,, 1]), to_linear(encoded))
  expect_equal(mapped[,, 4], image[,, 4])
  expect_identical(attr(mapped, "colorspace"), attr(image, "colorspace"))
  expect_identical(attr(mapped, "white_current"), attr(image, "white_current"))

  displayed = render_gamma_linear(mapped, srgb_to_linear = FALSE)
  expect_equal(as.numeric(displayed[,, 1]), encoded, tolerance = 1e-12)
  expect_equal(as.numeric(displayed[,, 4]), as.numeric(image[,, 4]))
  raster = prepare_native_raster(mapped)$native_raster
  expected = as.numeric(t(matrix(encoded, 2, 4)))
  expect_lte(
    max(abs(as.numeric(bitwAnd(as.integer(raster), 255L)) / 255 - expected)),
    1 / 255
  )

  filename = tempfile(fileext = ".png")
  on.exit(unlink(filename))
  ray_write_image(mapped, filename)
  written = png::readPNG(filename)
  expect_lte(max(abs(as.numeric(written[,, 1]) - encoded)), 1 / 255)
  expect_lte(
    max(abs(as.numeric(written[,, 4]) - as.numeric(image[,, 4]))),
    1 / 255
  )
})

test_that("raw tone mapping preserves linear HDR pixels", {
  image = ray_read_image(array(c(0, 0.18, 1, 8), c(2, 2, 3)))
  mapped = render_tonemap(image, method = "raw")
  expect_equal(unclass(mapped)[,, 1:3], unclass(image)[,, 1:3])
  expect_true(attr(mapped, "source_linear"))
})
