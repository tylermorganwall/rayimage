test_that("render_exposure bakes exposure and ISO into pixels", {
	img = ray_read_image(array(0.25, dim = c(2, 2, 4)))
	img[,, 4] = 0.5

	out = render_exposure(img, exposure = 1, iso = 200)

	expect_equal(unclass(out)[,, 1:3], array(1, dim = c(2, 2, 3)))
	expect_equal(unclass(out)[,, 4], matrix(0.5, 2, 2))
	expect_equal(attr(out, "exposure"), 1)
	expect_equal(attr(out, "iso"), 200)
})

test_that("plot and write do not apply informational exposure metadata", {
	img = ray_read_image(array(0.1, dim = c(2, 2, 3)))
	out = render_exposure(img, exposure = 1, iso = 200)

	tmp = tempfile(fileext = ".png")
	ray_write_image(out, tmp, write_linear = TRUE)
	written = png::readPNG(tmp)

	expect_equal(written[,, 1:3], array(0.4, dim = c(2, 2, 3)), tolerance = 1 / 255)
})

test_that("print.rayimg reports non-neutral exposure and ISO", {
	img = render_exposure(array(0.1, dim = c(2, 2, 3)), exposure = 1, iso = 200)

	printed = capture.output(print(img))

	expect_true(any(grepl("Exposure: \\+1 EV", printed)))
	expect_true(any(grepl("ISO: 200", printed)))
})

test_that("render_exposure auto uses luminance instead of max channel", {
	img = array(0, dim = c(2, 2, 3))
	img[,, 2] = 1
	img = ray_read_image(img)
	luma_weight = CS_ACESCG$rgb_to_xyz[2L, 2L]

	out = render_exposure(img, auto = TRUE, percentile = 1)

	expect_equal(attr(out, "exposure"), -log2(luma_weight))
	expect_equal(max(unclass(out)[,, 2]), 1 / luma_weight)
	expect_equal(max(unclass(out)[,, 2]) * luma_weight, 1)
})

test_that("render_exposure auto uses luminance percentile", {
	img = matrix(c(0.25, 0.25, 0.25, 100), nrow = 2)
	target_luminance = as.numeric(stats::quantile(as.numeric(img), 0.75, names = FALSE))

	out = render_exposure(img, auto = TRUE, percentile = 0.75)

	expect_equal(attr(out, "exposure"), -log2(target_luminance))
	expect_equal(
		as.numeric(stats::quantile(as.numeric(unclass(out)), 0.75, names = FALSE)),
		1
	)
	expect_gt(max(unclass(out)), 1)
})

test_that("render_exposure auto accounts for ISO and exposure compensation", {
	img = ray_read_image(array(0.25, dim = c(2, 2, 3)))

	out = render_exposure(img, iso = 200, auto = TRUE, percentile = 1)
	biased = render_exposure(img, exposure = 1, iso = 200, auto = TRUE, percentile = 1)

	expect_equal(attr(out, "exposure"), 1)
	expect_equal(max(unclass(out)), 1)
	expect_equal(attr(biased, "exposure"), 2)
	expect_equal(max(unclass(biased)), 2)
})

test_that("render_exposure auto verbose reports exposure info", {
	img = ray_read_image(array(0.25, dim = c(2, 2, 3)))

	expect_silent(render_exposure(img, auto = TRUE, percentile = 1))
	expect_message(
		render_exposure(img, auto = TRUE, percentile = 1, verbose = TRUE),
		"Auto exposure: \\+2.000 EV"
	)
})

test_that("ray_read_image can reset camera metadata", {
	img = render_exposure(array(0.25, dim = c(2, 2, 3)), exposure = 1, iso = 200)

	kept = ray_read_image(img)
	reset = ray_read_image(img, reset_camera_settings = TRUE)

	expect_equal(attr(kept, "exposure"), 1)
	expect_equal(attr(kept, "iso"), 200)
	expect_equal(attr(reset, "exposure"), 0)
	expect_equal(attr(reset, "iso"), 100)
	expect_equal(as.numeric(reset), as.numeric(img))
})

test_that("combining operations reset camera metadata", {
	img = render_exposure(array(0.25, dim = c(2, 2, 3)), exposure = 1, iso = 200)
	overlay = array(0, dim = c(2, 2, 4))
	overlay[,, 4] = 0.5

	out = render_image_overlay(img, overlay)

	expect_equal(attr(out, "exposure"), 0)
	expect_equal(attr(out, "iso"), 100)
})

test_that("colorspace and geometric operations preserve camera metadata", {
	img = render_exposure(array(0.25, dim = c(4, 4, 3)), exposure = 1, iso = 200)

	converted = render_convert_colorspace(img, to_mats = CS_SRGB)
	cropped = render_crop(img, width_limits = c(0, 0.5), height_limits = c(0, 0.5))

	expect_equal(attr(converted, "exposure"), 1)
	expect_equal(attr(converted, "iso"), 200)
	expect_equal(attr(cropped, "exposure"), 1)
	expect_equal(attr(cropped, "iso"), 200)
})
