test_that("render_text_image trims glyph bounds and adds padding", {
  old_device = getOption("device")
  on.exit(options(device = old_device), add = TRUE)
  options(
    device = function(...)
      grDevices::pdf(file = tempfile(fileext = ".pdf"), ...)
  )

  args = list(
    text = "A",
    size = 40,
    width = 160,
    height = 160,
    background_alpha = 0,
    check_text_width = FALSE,
    check_text_height = FALSE
  )

  loose = do.call(render_text_image, c(args, list(trim = FALSE)))
  tight = do.call(render_text_image, c(args, list(trim = TRUE)))
  padded = do.call(
    render_text_image,
    c(args, list(trim = TRUE, trim_padding = 5))
  )
  implied_trim = do.call(
    render_text_image,
    c(args, list(trim = FALSE, trim_padding = c(1, 2, 3, 4)))
  )

  expect_true(dim(tight)[1] < dim(loose)[1])
  expect_true(dim(tight)[2] < dim(loose)[2])
  expect_equal(dim(padded)[1:2], dim(tight)[1:2] + 10)
  expect_equal(dim(implied_trim)[1:2], dim(tight)[1:2] + c(4, 6))

  padded_array = unclass(padded)
  expect_true(all(
    padded_array[c(1:5, (nrow(padded_array) - 4):nrow(padded_array)), , 4] == 0
  ))
  expect_true(all(
    padded_array[, c(1:5, (ncol(padded_array) - 4):ncol(padded_array)), 4] == 0
  ))
})
