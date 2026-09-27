test_that("taglines occupy the requested corner with exact padding", {
  image = array(0, c(90, 220, 3))
  for (corner in c("bottomright", "bottomleft", "topright", "topleft")) {
    result = render_tagline(
      image,
      "Tag",
      corner = corner,
      size = 12,
      padding = 7,
      alpha = 1
    )
    expect_equal(dim(result), dim(image))
    changed = which(result[,, 1] > 0, arr.ind = TRUE)
    expect_gt(nrow(changed), 0)
    if (corner %in% c("bottomright", "bottomleft")) {
      expect_equal(max(changed[, 1]), 83)
    } else {
      expect_equal(min(changed[, 1]), 8)
    }
    if (corner %in% c("bottomright", "topright")) {
      expect_equal(max(changed[, 2]), 213)
    } else {
      expect_equal(min(changed[, 2]), 8)
    }
  }
})

test_that("taglines preserve alpha outside text and accept files", {
  image = array(.2, c(90, 220, 4))
  image[,, 4] = .4
  result = render_tagline(image, "Tag", size = 12)
  expect_equal(dim(result), dim(image))
  expect_equal(as.numeric(result[1:30, , ]), as.numeric(image[1:30, , ]))
  expect_gt(max(result[,, 4]), .4)
  invisible_text = render_tagline(image, "Tag", alpha = 0)
  expect_equal(as.numeric(invisible_text), as.numeric(image))
  input = tempfile(fileext = ".png")
  output = tempfile(fileext = ".png")
  on.exit(unlink(c(input, output)), add = TRUE)
  ray_write_image(image, input)
  render_tagline(input, "Created with rayrender", filename = output)
  expect_true(file.exists(output))
  expect_equal(dim(png::readPNG(output))[1:2], c(90L, 220L))
})

test_that("taglines reject invalid or clipped text", {
  image = array(0, c(90, 220, 3))
  expect_error(render_tagline(image, "a\nb"), "one nonempty line")
  expect_error(render_tagline(image, " "), "one nonempty line")
  expect_error(render_tagline(image, "Tag", corner = "center"), "arg")
  expect_error(render_tagline(image, "Tag", size = 0), "positive")
  expect_error(
    render_tagline(image, "Tag", padding = -.5),
    "nonnegative integer"
  )
  expect_error(render_tagline(image, "Tag", alpha = 2), "between zero and one")
  expect_error(render_tagline(image, "Too large", size = 100), "does not fit")
  expect_equal(eval(formals(render_tagline)$font), "mono")
  expect_equal(dim(render_tagline(image, "Tag")), dim(image))
})
