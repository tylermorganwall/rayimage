#'@title Make Vignette Overlay
#'
#'@description Makes an overlay to simulate vignetting in a camera
#'
#'@param width Width of the image.
#'@param height Height of the image.
#'@param intensity Default `0.4`. `1` is max intensity, `0` is min.
#'@param radius Default `NULL`. Max of height or width, divided by 2.
#'@param radius_multiplier Default `1.3`.
#'@param color Default `black`.
#'@keywords internal
make_vignette_overlay = function(width, height, intensity=0.3, radius_multiplier = 1.3,
                                 radius=NULL,color = "#000000") {
  value = array(0, dim = c(floor(width*radius_multiplier),floor(height*radius_multiplier),4))
  hex_color = convert_color(color,as_hex = TRUE)
  color = convert_color(color)
  value[,,4] = gen_ellipse(intensity, floor(width*radius_multiplier), floor(height*radius_multiplier))
  value[,,1] = color[1]
  value[,,2] = color[2]
  value[,,3] = color[3]

  tempcircle = tempfile(fileext = ".png")
  png::writePNG(value,tempcircle)
  imageval = magick::image_read(tempcircle)
  if(radius_multiplier >= 1) {
    imageval |>
      magick::image_blur(radius=radius/2, sigma = radius/4) |>
      magick::image_crop(sprintf("%ix%i+%i+%i",
                                 height,
                                 width,
                                 floor((height*radius_multiplier-height)/2),
                                 floor((width*radius_multiplier-width)/2))) |>
      magick::image_write(tempcircle)
  } else {
    imageval |>
      magick::image_border(geometry = sprintf("%ix%i",(height-floor(height*radius_multiplier))/2,(width-floor(width*radius_multiplier))/2), color=hex_color) |>
      magick::image_blur(radius=radius/2, sigma = radius/4) |>
      magick::image_write(tempcircle)
  }
  return(tempcircle)
}
