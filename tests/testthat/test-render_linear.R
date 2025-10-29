srgb_to_linear_ref = function(x) {
	idx = x <= 0.04045
	out = x
	out[idx] = x[idx] / 12.92
	out[!idx] = ((x[!idx] + 0.055) / 1.055)^2.4
	out
}

test_that("render_linear converts numeric RGB vectors", {
	srgb = c(0.02, 0.5, 0.9)
	expected = srgb_to_linear_ref(srgb)
	result = render_linear(srgb)
	expect_length(result, 3L)
	expect_equal(result, expected, tolerance = 1e-7)
})

test_that("render_linear converts hex colors", {
	srgb = c(0x12, 0x34, 0x56) / 255
	expected = srgb_to_linear_ref(srgb)
	result = render_linear("#123456")
	expect_equal(result, expected, tolerance = 1e-7)
})

test_that("render_linear handles multiple hex colors", {
	cols = c("#123456", "#abcdef")
	matrix_rgb = grDevices::col2rgb(cols) / 255
	expected = t(srgb_to_linear_ref(matrix_rgb))
	result = render_linear(cols)
	expect_equal(dim(result), c(2L, 3L))
	expect_equal(result, expected, tolerance = 1e-7)
})

test_that("render_linear converts rayimg data and preserves alpha", {
	srgb_pixels = array(
		c(
			0.25, 0.5, 0.75, 0.4,
			0.1, 0.3, 0.6, 0.8
		),
		dim = c(1, 2, 4)
	)
	img = rayimage:::rayimg(
		srgb_pixels,
		source_linear = FALSE,
		colorspace = CS_SRGB,
		white_current = CS_SRGB$white_xyz
	)

	linear_img = render_linear(img)

	expect_s3_class(linear_img, "rayimg")
	expect_true(isTRUE(attr(linear_img, "source_linear")))

	expected_rgb = srgb_to_linear_ref(srgb_pixels[,, 1:3, drop = FALSE])
	expect_equal(
		linear_img[,, 1:3, drop = FALSE],
		expected_rgb,
		tolerance = 1e-7,
		ignore_attr = TRUE
	)
	expect_equal(
		linear_img[,, 4],
		srgb_pixels[,, 4],
		tolerance = 1e-7,
		ignore_attr = TRUE
	)

	expect_identical(render_linear(linear_img), linear_img)
})

test_that("render_linear validates unsupported inputs", {
	expect_error(render_linear(c(0.5, 0.5)), "length-3 RGB vector")
	expect_error(render_linear(c(0.5, 0.5, 1.2)), "between 0 and 1")
	expect_error(render_linear(c(0.5, NA, 0.5)), "finite")
	expect_error(render_linear(list(0.1, 0.2, 0.3)), "unsupported input")
})

test_that("render_linear handles matrices and arrays", {
	mat = matrix(c(0.1, 0.5, 0.9, 0.3), nrow = 2)
	expected_mat = srgb_to_linear_ref(mat)
	expect_equal(render_linear(mat), expected_mat, tolerance = 1e-7)

	arr_two = array(
		c(0.2, 0.7, 0.4, 0.9),
		dim = c(2, 1, 2)
	)
	res_two = render_linear(arr_two)
	expect_equal(res_two[,, 1], srgb_to_linear_ref(arr_two[,, 1]), tolerance = 1e-7)
	expect_equal(res_two[,, 2], arr_two[,, 2], tolerance = 1e-7)

	arr_four = array(
		c(
			0.2, 0.5, 0.8, 0.9,
			0.1, 0.3, 0.6, 0.7,
			0.4, 0.2, 0.9, 0.1
		),
		dim = c(1, 3, 4)
	)
	res_four = render_linear(arr_four)
	expect_equal(
		res_four[,, 1:3, drop = FALSE],
		srgb_to_linear_ref(arr_four[,, 1:3, drop = FALSE]),
		tolerance = 1e-7
	)
	expect_equal(res_four[,, 4], arr_four[,, 4], tolerance = 1e-7)

	arr_4d = array(0, dim = c(2, 2, 2, 3))
	expect_error(
		render_linear(arr_4d),
		"expected a 2D matrix or 3D array"
	)
})
