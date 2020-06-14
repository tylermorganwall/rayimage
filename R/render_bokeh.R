#'@title Render Bokeh
#'
#'@description Takes an image and a depth map to render the image with depth of field
#'(i.e. similar to "Portrait Mode" in an iPhone). User can specify a custom bokeh shape,
#'or use one of the built-in bokeh types.
#'
#'@param image Image filename or 3-layer RGB array.
#'@param depthmap Depth map filename or 1d array.
#'@param focus Defaults `0.5`. Depth in which to blur. Minimum 0, maximum 1.
#'@param focallength Default `100`. Focal length of the virtual camera.
#'@param fstop Default `4`. F-stop of the virtual camera.
#'@param filename Default `NULL`. The filename of the image to be saved. If this is not given, the image will be plotted instead.
#'@param preview Default `TRUE`. If `FALSE`, it will not display the image and just return the RGB array.
#'@param preview_focus Default `FALSE`. If `TRUE`, a red line will be drawn across the image
#'showing where the camera will be focused.
#'@param bokehshape Default `circle`. Also built-in: `hex`. The shape of the bokeh. If the user
#'passes in a 2D matrix, that matrix will control the shape of the bokeh.
#'@param bokehintensity Default `1`. Intensity of the bokeh when the pixel intensity is greater than `bokehlimit`.
#'@param bokehlimit Default `0.8`. Limit after which the bokeh intensity is increased by `bokehintensity`.
#'@param rotation Default `0`. Number of degrees to rotate the hexagonal bokeh shape.
#'@param aberration Default `0`. Adds chromatic aberration to the image. Maximum of `1`.
#'@param gamma_correction Default `TRUE`. Controls gamma correction when adding colors. Default exponent of 2.2.
#'@param progress Default `TRUE`. Whether to display a progress bar.
#'@return 3-layer RGB array of the processed image.
#'@export
#'@examples
#'#Plot the dragon
#'plot_image(dragon)
#'
#'#Plot the depth map
#'image(dragondepth, asp = 1, col = grDevices::heat.colors(256))
#'
#'#Preview the focal plane:
#'\donttest{
#'render_bokeh(dragon, dragondepth, focus=950, preview_focus = TRUE)
#'}
#'#Change the focal length:
#'\donttest{
#'render_bokeh(dragon, dragondepth, focus=950, focallength=300)
#'}
#'#Add chromatic aberration:
#'\donttest{
#'render_bokeh(dragon, dragondepth, focus=950, focallength=300, aberration = 0.5)
#'}
#'#Change the focal distance:
#'\donttest{
#'render_bokeh(dragon, dragondepth, focus=600, focallength=300)
#'render_bokeh(dragon, dragondepth, focus=1300, focallength=300)
#'}
#'#Change the bokeh shape to a hexagon:
#'\donttest{
#'render_bokeh(dragon, dragondepth, bokehshape = "hex",
#'             focallength=300, focus=600)
#'}
#'#Change the bokeh intensity:
#'\donttest{
#'render_bokeh(dragon, dragondepth,
#'             focallength=400, focus=900, bokehintensity = 1)
#'render_bokeh(dragon, dragondepth,
#'             focallength=400, focus=900, bokehintensity = 3)
#'}
#'#Rotate the hexagonal shape:
#'\donttest{
#'render_bokeh(dragon, dragondepth, bokehshape = "hex", rotation=15,
#'             focallength=300, focus=600)
#'}
render_bokeh = function(image, depthmap,
                        focus=0.5, focallength = 100, fstop = 4, filename=NULL,
                        preview = TRUE, preview_focus = FALSE,
                        bokehshape = "circle", bokehintensity = 1, bokehlimit = 0.8, rotation=0,
                        aberration = 0, gamma_correction = TRUE, progress = interactive()) {
  if(!is.null(filename)) {
    if(tools::file_ext(filename) != "png") {
      filename = paste0(filename,".png")
    }
  }
  imagetype = get_file_type(image)
  if(imagetype == "array") {
    temp_image = aperm(image,c(2,1,3))
  }
  depthtype = get_file_type(depthmap)
  if(depthtype == "array") {
    depthmap = flipud(aperm(depthmap,c(2,1,3)))
  }
  if(depthtype == "matrix") {
    depthmap = flipud(t(depthmap))
  }
  if(preview_focus) {
    preview_focus(image, depthmap, focus, imagetype, depthtype)
    return(invisible())
  }
  if(is.matrix(bokehshape)) {
    custombokeh = bokehshape
    bokehshape = 2L
  } else {
    if(bokehshape == "circle") {
      bokehshape = 0L
    } else {
      bokehshape = 1L
    }
    custombokeh = matrix(1,1,1)
  }
  if(aberration >= 1 || aberration <= -1) {
    stop("aberration value must be less than 1 and greater than -1")
  }
  #Load and rotate images if png
  if(imagetype == "jpg") {
    temp_image = suppressWarnings(aperm(jpeg::readJPEG(image),c(2,1,3)))
  } else if (imagetype == "png"){
    temp_image = suppressWarnings(aperm(png::readPNG(image),c(2,1,3)))
  }
  if(depthtype == "jpg") {
    depthmap = suppressWarnings(jpeg::readJPEG(depthmap))
  } else if (depthtype == "png"){
    depthmap = suppressWarnings(png::readPNG(depthmap))
  }
  if(length(dim(depthmap)) == 3) {
    depthmap = depthmap[,,1]
  }

  depthmap[is.na(depthmap)] = max(depthmap, na.rm = TRUE)*2
  if(gamma_correction) {
    temp_image = temp_image^2.2
  }
  for(i in 1:3) {
    if(i == 1) {
      depthmap2 = calc_bokeh_size(depthmap,focus,focallength, fstop,1+aberration)
    } else if(i ==2) {
      depthmap2 = calc_bokeh_size(depthmap,focus,focallength, fstop,1)
    } else {
      depthmap2 = calc_bokeh_size(depthmap,focus,focallength, fstop,1-aberration)
    }
    temp_image[,,i] = flipud(t(psf(t(flipud(temp_image[,,i])),depthmap2,
                                depthmap, focus, bokehshape, custombokeh = custombokeh,
                                bokehintensity, bokehlimit, rotation, progbar = progress,channel = i)))
  }
  if(gamma_correction) {
    temp_image = temp_image ^ (1/2.2)
  }
  temp_image[temp_image > 1] = 1
  temp_image[temp_image < 0] = 0
  if(is.null(filename)) {
    if(!preview) {
      return(aperm(temp_image,c(2,1,3)))
    }
    plot_image(aperm(temp_image,c(2,1,3)))
    return(invisible(aperm(temp_image,c(2,1,3))))
  } else {
    save_png(aperm(temp_image,c(2,1,3)),filename)
    return(invisible(aperm(temp_image,c(2,1,3))))
  }
}
