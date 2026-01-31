test_that("DNG roundtrip mosaic preserves values", {
	h = 16L
	w = 16L
	mosaic = matrix(seq(0, 1, length.out = h * w), nrow = h, ncol = w)
	attr(mosaic, "dng") = list(
		cfa_pattern = matrix(c(0, 1, 1, 2), 2, 2, byrow = TRUE),
		black_level = c(0, 0, 0, 0),
		white_level = c(65535, 65535, 65535, 65535)
	)

	tmp = tempfile(fileext = ".dng")
	ray_write_image(mosaic, tmp)
	out = ray_read_image(tmp)

	diff = max(abs(as.matrix(out) - mosaic))
	expect_true(diff <= (1 / 65535 + 1e-6))
})
