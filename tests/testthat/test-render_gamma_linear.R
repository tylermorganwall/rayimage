test_that("render_gamma_linear round-trips numeric vectors", {
	srgb = c(0.2, 0.4, 0.6)
	linear = render_gamma_linear(srgb)
	expect_type(linear, "double")
	expect_equal(
		render_gamma_linear(linear, srgb_to_linear = FALSE),
		srgb,
		tolerance = 1e-7
	)
	expect_equal(
		render_gamma_linear(linear, srgb_to_linear = FALSE, as_hex = TRUE),
		rayimage:::rgb_to_hex(matrix(srgb, ncol = 3, byrow = TRUE))
	)
})

test_that("render_gamma_linear handles hex colors", {
	hex_single = "#123456"
	linear_single = render_gamma_linear(hex_single)
	expect_equal(
		render_gamma_linear(linear_single, srgb_to_linear = FALSE, as_hex = TRUE),
		toupper(hex_single)
	)

	hex_multi = c("#123456", "#ABCDEF")
	linear_multi = render_gamma_linear(hex_multi)
	expected_srgb = t(grDevices::col2rgb(hex_multi) / 255)
	expect_equal(
		render_gamma_linear(linear_multi, srgb_to_linear = FALSE),
		expected_srgb,
		tolerance = 1e-7
	)
	back_hex = vapply(
		seq_len(nrow(linear_multi)),
		function(i) {
			render_gamma_linear(
				linear_multi[i, ],
				srgb_to_linear = FALSE,
				as_hex = TRUE
			)
		},
		character(1)
	)
	expect_equal(toupper(hex_multi), back_hex)
})

test_that("render_gamma_linear round-trips matrices", {
	grey_mat = matrix(c(0.1, 0.5, 0.9, 0.3), nrow = 2)
	linear_mat = render_gamma_linear(grey_mat)
	expect_equal(
		render_gamma_linear(linear_mat, srgb_to_linear = FALSE),
		grey_mat,
		tolerance = 1e-7
	)
	g_vals = as.vector(grey_mat)
	expected_hex = rayimage:::rgb_to_hex(cbind(g_vals, g_vals, g_vals))
	expect_equal(
		render_gamma_linear(linear_mat, srgb_to_linear = FALSE, as_hex = TRUE),
		expected_hex
	)
})

test_that("render_gamma_linear round-trips arrays of all channel counts", {
	# Greyscale
	arr1 = array(c(0.2, 0.6, 0.4, 0.8), dim = c(2, 2, 1))
	lin1 = render_gamma_linear(arr1)
	expect_equal(
		render_gamma_linear(lin1, srgb_to_linear = FALSE),
		arr1,
		tolerance = 1e-7,
		ignore_attr = TRUE
	)
	g_vals = as.vector(arr1[,, 1])
	expected1 = rayimage:::rgb_to_hex(cbind(g_vals, g_vals, g_vals))
	expect_equal(
		render_gamma_linear(lin1, srgb_to_linear = FALSE, as_hex = TRUE),
		expected1
	)

	# Greyscale + alpha
	arr2 = array(
		c(
			0.3,
			0.7,
			0.5,
			0.9,
			0.4,
			0.2,
			0.6,
			0.1
		),
		dim = c(2, 2, 2)
	)
	lin2 = render_gamma_linear(arr2)
	back2 = render_gamma_linear(lin2, srgb_to_linear = FALSE)
	expect_equal(back2[,, 1], arr2[,, 1], tolerance = 1e-7)
	expect_equal(back2[,, 2], arr2[,, 2], tolerance = 1e-7)
	g2 = as.vector(arr2[,, 1])
	a2 = as.vector(arr2[,, 2])
	expected2 = rayimage:::rgb_to_hex(cbind(g2, g2, g2, a2))
	expect_equal(
		render_gamma_linear(lin2, srgb_to_linear = FALSE, as_hex = TRUE),
		expected2
	)

	# RGB
	arr3 = array(
		c(
			0.2,
			0.4,
			0.6,
			0.7,
			0.1,
			0.3,
			0.5,
			0.8,
			0.2,
			0.9,
			0.3,
			0.4
		),
		dim = c(2, 2, 3)
	)
	lin3 = render_gamma_linear(arr3)
	expect_equal(
		render_gamma_linear(lin3, srgb_to_linear = FALSE),
		arr3,
		tolerance = 1e-7,
		ignore_attr = TRUE
	)
	r3 = as.vector(arr3[,, 1])
	g3 = as.vector(arr3[,, 2])
	b3 = as.vector(arr3[,, 3])
	expected3 = rayimage:::rgb_to_hex(cbind(r3, g3, b3))
	expect_equal(
		render_gamma_linear(lin3, srgb_to_linear = FALSE, as_hex = TRUE),
		expected3
	)

	# RGBA
	arr4 = array(
		c(
			0.2,
			0.4,
			0.6,
			0.5,
			0.7,
			0.1,
			0.3,
			0.8,
			0.5,
			0.8,
			0.2,
			0.4,
			0.9,
			0.3,
			0.4,
			0.6
		),
		dim = c(2, 2, 4)
	)
	lin4 = render_gamma_linear(arr4)
	back4 = render_gamma_linear(lin4, srgb_to_linear = FALSE)
	expect_equal(
		back4[,, 1:3, drop = FALSE],
		arr4[,, 1:3, drop = FALSE],
		tolerance = 1e-7
	)
	expect_equal(back4[,, 4], arr4[,, 4], tolerance = 1e-7)
	r4 = as.vector(arr4[,, 1])
	g4 = as.vector(arr4[,, 2])
	b4 = as.vector(arr4[,, 3])
	a4 = as.vector(arr4[,, 4])
	expected4 = rayimage:::rgb_to_hex(cbind(r4, g4, b4, a4))
	expect_equal(
		render_gamma_linear(lin4, srgb_to_linear = FALSE, as_hex = TRUE),
		expected4
	)
})

test_that("render_gamma_linear round-trips rayimg inputs and preserves metadata", {
	rgba_pixels = array(
		c(
			0.2,
			0.4,
			0.6,
			0.5,
			0.7,
			0.1,
			0.3,
			0.8
		),
		dim = c(1, 2, 4)
	)
	img = rayimage:::rayimg(
		rgba_pixels,
		source_linear = FALSE,
		colorspace = CS_SRGB,
		white_current = CS_SRGB$white_xyz
	)

	linear_img = render_gamma_linear(img)
	expect_true(isTRUE(attr(linear_img, "source_linear")))
	expect_equal(
		linear_img[,, 4],
		img[,, 4],
		tolerance = 1e-7,
		ignore_attr = TRUE
	)

	back_img = render_gamma_linear(linear_img, srgb_to_linear = FALSE)
	expect_false(isTRUE(attr(back_img, "source_linear")))
	expect_equal(
		unclass(back_img),
		unclass(img),
		tolerance = 1e-7,
		ignore_attr = TRUE
	)
	expect_identical(attr(back_img, "colorspace"), attr(img, "colorspace"))
	expect_identical(attr(back_img, "white_current"), attr(img, "white_current"))

	r_vals = as.vector(rgba_pixels[,, 1])
	g_vals = as.vector(rgba_pixels[,, 2])
	b_vals = as.vector(rgba_pixels[,, 3])
	a_vals = as.vector(rgba_pixels[,, 4])
	expected_hex = rayimage:::rgb_to_hex(cbind(r_vals, g_vals, b_vals, a_vals))
	hex_from_linear = render_gamma_linear(
		linear_img,
		srgb_to_linear = FALSE,
		as_hex = TRUE
	)
	expect_equal(hex_from_linear, expected_hex)
})
