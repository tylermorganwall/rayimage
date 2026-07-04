skip_unless_libopenexr_metadata = function() {
  skip_if_not_installed("libopenexr")
  if (!"metadata" %in% names(formals(libopenexr::write_exr))) {
    skip("libopenexr metadata support is not available")
  }
}

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

test_that("ray_write_image writes rayimg color metadata to EXR", {
  skip_on_cran()
  skip_unless_libopenexr_metadata()

  rgba = array(0.25, dim = c(2, 2, 4))
  rgba[,, 4] = 1
  img = rayimage:::rayimg(
    rgba,
    source_linear = TRUE,
    colorspace = CS_P3D65,
    white_current = rayimage:::get_whitepoint_xyz("D50")$value
  )
  attr(img, "exr") = list(whiteLuminance = 100)

  tmp = tempfile(fileext = ".exr")
  on.exit(unlink(tmp), add = TRUE)

  expect_no_error(ray_write_image(
    img,
    tmp,
    metadata = list(envmap = "latlong")
  ))
  metadata = libopenexr::read_exr(tmp)$metadata

  expect_equal(
    unname(metadata$chromaticities$red),
    CS_P3D65$primaries$r,
    tolerance = 1e-7
  )
  expect_equal(
    unname(metadata$chromaticities$green),
    CS_P3D65$primaries$g,
    tolerance = 1e-7
  )
  expect_equal(
    unname(metadata$chromaticities$blue),
    CS_P3D65$primaries$b,
    tolerance = 1e-7
  )
  expect_equal(
    unname(metadata$chromaticities$white),
    rayimage:::xyz_to_xy(CS_P3D65$white_xyz),
    tolerance = 1e-7
  )
  expect_equal(
    unname(metadata$adoptedNeutral),
    rayimage:::xyz_to_xy(rayimage:::get_whitepoint_xyz("D50")$value),
    tolerance = 1e-7
  )
  expect_equal(metadata$whiteLuminance, 100)
  expect_equal(metadata$envmap, "latlong")
})

test_that("ray_read_image uses EXR color metadata when present", {
  skip_on_cran()
  skip_unless_libopenexr_metadata()

  r = matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2)
  g = matrix(0.5, nrow = 2, ncol = 2)
  b = matrix(0.8, nrow = 2, ncol = 2)
  metadata = list(
    chromaticities = list(
      red = CS_BT2020$primaries$r,
      green = CS_BT2020$primaries$g,
      blue = CS_BT2020$primaries$b,
      white = rayimage:::xyz_to_xy(CS_BT2020$white_xyz)
    ),
    adoptedNeutral = rayimage:::xyz_to_xy(
      rayimage:::get_whitepoint_xyz("D50")$value
    ),
    whiteLuminance = 203
  )

  tmp = tempfile(fileext = ".exr")
  on.exit(unlink(tmp), add = TRUE)
  libopenexr::write_exr(tmp, r, g, b, metadata = metadata)

  img = ray_read_image(tmp)
  colorspace = attr(img, "colorspace")

  expect_equal(colorspace$primaries$r, CS_BT2020$primaries$r, tolerance = 1e-7)
  expect_equal(colorspace$primaries$g, CS_BT2020$primaries$g, tolerance = 1e-7)
  expect_equal(colorspace$primaries$b, CS_BT2020$primaries$b, tolerance = 1e-7)
  expect_equal(colorspace$white_xyz, CS_BT2020$white_xyz, tolerance = 1e-5)
  expect_equal(
    attr(img, "white_current"),
    rayimage:::get_whitepoint_xyz("D50")$value,
    tolerance = 1e-5
  )
  expect_equal(attr(img, "exr")$whiteLuminance, 203)
  expect_null(attr(img, "exr")$chromaticities)
  expect_null(attr(img, "exr")$adoptedNeutral)
  expect_identical(attr(ray_read_image(img), "exr"), attr(img, "exr"))
  expect_identical(attr(img[,, 1:3], "exr"), attr(img, "exr"))
})

test_that("ray_read_image does not duplicate promoted EXR color metadata", {
  skip_on_cran()
  skip_unless_libopenexr_metadata()

  r = matrix(0.1, nrow = 2, ncol = 2)
  g = matrix(0.5, nrow = 2, ncol = 2)
  b = matrix(0.8, nrow = 2, ncol = 2)
  metadata = list(
    chromaticities = list(
      red = CS_SRGB$primaries$r,
      green = CS_SRGB$primaries$g,
      blue = CS_SRGB$primaries$b,
      white = rayimage:::xyz_to_xy(CS_SRGB$white_xyz)
    ),
    adoptedNeutral = rayimage:::xyz_to_xy(CS_SRGB$white_xyz)
  )

  tmp = tempfile(fileext = ".exr")
  on.exit(unlink(tmp), add = TRUE)
  libopenexr::write_exr(tmp, r, g, b, metadata = metadata)

  img = ray_read_image(tmp)

  expect_equal(
    attr(img, "colorspace")$primaries,
    CS_SRGB$primaries,
    tolerance = 1e-7
  )
  expect_equal(attr(img, "white_current"), CS_SRGB$white_xyz, tolerance = 1e-5)
  expect_null(attr(img, "exr", exact = TRUE))
})

test_that("ray_read_image falls back for EXR files without color metadata", {
  skip_on_cran()
  skip_unless_libopenexr_metadata()

  r = matrix(0.1, nrow = 2, ncol = 2)
  g = matrix(0.5, nrow = 2, ncol = 2)
  b = matrix(0.8, nrow = 2, ncol = 2)

  tmp = tempfile(fileext = ".exr")
  on.exit(unlink(tmp), add = TRUE)
  libopenexr::write_exr(tmp, r, g, b)

  img = ray_read_image(tmp)

  expect_equal(attr(img, "colorspace"), CS_ACESCG)
  expect_equal(attr(img, "white_current"), CS_ACESCG$white_xyz)
  expect_null(attr(img, "exr", exact = TRUE))
})
