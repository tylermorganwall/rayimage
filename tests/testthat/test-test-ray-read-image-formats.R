test_that("common 8-bit formats via magick can be read and plotted", {
  skip_on_cran()
  skip_if_not_installed("magick")

  generate_and_test = function(ext, magick_format) {
    img = magick::image_blank(width = 16, height = 16, color = "red")
    tmp = tempfile(fileext = paste0(".", ext))

    write_res = try(
      magick::image_write(img, path = tmp, format = magick_format),
      silent = TRUE
    )
    if (inherits(write_res, "try-error")) {
      skip(paste("magick/ImageMagick not built with", magick_format, "support"))
    }

    ri = ray_read_image(tmp, convert_to_array = TRUE)
    expect_s3_class(ri, "rayimg")

    out = tempfile(fileext = ".png")
    grDevices::png(out, width = 16, height = 16)
    expect_silent(plot_image(ri))
    grDevices::dev.off()
    expect_true(file.exists(out))
  }

  formats = list(
    list(ext = "jpg", magick_format = "jpeg"), # JPEG
    list(ext = "png", magick_format = "png"), # PNG
    list(ext = "tif", magick_format = "tiff"), # TIFF
    list(ext = "tga", magick_format = "tga"), # TGA
    list(ext = "bmp", magick_format = "bmp"), # BMP
    list(ext = "psd", magick_format = "psd"), # PSD
    list(ext = "gif", magick_format = "gif"), # GIF
    list(ext = "pnm", magick_format = "pnm") # PNM (PBM/PGM/PPM family)
  )

  for (f in formats) {
    generate_and_test(ext = f$ext, magick_format = f$magick_format)
  }
})

test_that("Radiance HDR files can be read and plotted", {
  skip_on_cran()
  skip_if_not_installed("magick")

  img = magick::image_blank(width = 16, height = 16, color = "red")
  tmp = tempfile(fileext = ".hdr")

  write_res = try(
    magick::image_write(img, path = tmp, format = "hdr"),
    silent = TRUE
  )
  if (inherits(write_res, "try-error")) {
    skip("magick/ImageMagick not built with Radiance HDR support")
  }

  ri = ray_read_image(tmp, convert_to_array = TRUE)
  expect_s3_class(ri, "rayimg")

  out = tempfile(fileext = ".png")
  grDevices::png(out, width = 16, height = 16)
  expect_silent(plot_image(ri))
  grDevices::dev.off()
  expect_true(file.exists(out))
})

test_that("EXR files can be read and plotted", {
  skip_on_cran()
  skip_if_not_installed("libopenexr")

  w = 4L
  h = 4L
  r = matrix(seq(0, 1, length.out = w * h), nrow = h, ncol = w)
  g = matrix(0.5, nrow = h, ncol = w)
  b = matrix(1 - r, nrow = h, ncol = w)

  tmp = tempfile(fileext = ".exr")

  # libopenexr::write_exr(path, r, g, b, a)
  libopenexr::write_exr(
    tmp,
    r,
    g,
    b
  )

  ri = ray_read_image(tmp, convert_to_array = TRUE)
  expect_s3_class(ri, "rayimg")

  out = tempfile(fileext = ".png")
  grDevices::png(out, width = 16, height = 16)
  expect_silent(plot_image(ri))
  grDevices::dev.off()
  expect_true(file.exists(out))
})

test_that("Softimage PIC files can be read and plotted (when supported)", {
  skip_on_cran()
  skip_if_not_installed("magick")

  img = magick::image_blank(width = 16, height = 16, color = "red")
  tmp = tempfile(fileext = ".pic")

  write_res = try(
    magick::image_write(img, path = tmp, format = "pic"),
    silent = TRUE
  )
  if (inherits(write_res, "try-error")) {
    skip("magick/ImageMagick not built with Softimage PIC support")
  }

  ri = ray_read_image(tmp, convert_to_array = TRUE)
  expect_s3_class(ri, "rayimg")

  out = tempfile(fileext = ".png")
  grDevices::png(out, width = 16, height = 16)
  expect_silent(plot_image(ri))
  grDevices::dev.off()
  expect_true(file.exists(out))
})
