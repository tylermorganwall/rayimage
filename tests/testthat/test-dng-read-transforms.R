test_that("active area cropping reduces dims and matches subset", {
  img = array(seq_len(4 * 5), dim = c(4, 5))
  out = rayimage:::dng_apply_active_area_cpp(img, c(1, 1, 3, 4))
  expect_equal(dim(out), c(2, 3))
  expect_equal(out, img[2:3, 2:4])
})

test_that("exposure normalization scales values", {
  img = array(c(0.1, 0.2, 0.3, 0.4), dim = c(2, 2))
  out = rayimage:::dng_exposure_normalize_cpp(img, iso = 200, exposure_time = 0.01)
  scale = (100 / 200) * (1 / 0.01)
  expect_equal(out, img * scale)
})

test_that("baseline exposure undo reverses scaling", {
  img = array(seq_len(4), dim = c(2, 2))
  baseline = 1.0
  scaled = img * (2 ^ baseline)
  out = rayimage:::dng_undo_baseline_exposure_cpp(scaled, baseline)
  expect_equal(out, img)
})
