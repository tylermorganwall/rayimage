#'@title render_bokeh
#'
#'@description Takes an image and a depth map to render the image with depth of field.
#'
#'@param image Image filename or 3-layer RGB array.
#'@param depthmap Depth map filename or 1d array.
#'@param focus Defaults `0.5`. Depth in which to blur. Minimum 0, maximum 1.
#'@param focallength Default `100`. Focal length of the virtual camera.
#'@param fstop Default `4`. F-stop of the virtual camera.
#'@param filename The filename of the image to be saved. If this is not given, the image will be plotted instead.
#'@param bokehshape Default `circle`. Also built-in: `hex`. The shape of the bokeh.
#'@param bokehintensity Default `5`. Intensity of the bokeh when the pixel intensity is greater than `bokehlimit`.
#'@param bokehlimit Default `0.8`. Limit after which the bokeh intensity is increased by `bokehintensity`.
#'@param rotation Default `0`. Number of degrees to rotate the hexagonal bokeh shape.
#'@param gamma_correction Default `TRUE`. Controls gamma correction when adding colors. Default exponent of 2.2.
#'@param progress. Default `TRUE`. Whether to display a progress bar.
#'@export
#'@examples
#'
render_bokeh = function(image, depthmap, focus=0.5, focallength = 100,
                     fstop = 4, filename=NULL, bokehshape = "circle",
                     bokehintensity = 5, mindistance = 0.1, rotation=0,
                     gamma_correction = TRUE, progress = TRUE) {
  if(!is.null(filename)) {
    if(substring(filename, nchar(filename)-3,nchar(filename)) != ".png") {
      filename = paste0(filename,".png")
    }
  }
  if(is.character(image)) {
    if(substring(image, nchar(image)-3,nchar(image)) == ".png") {
      imagetype = "png"
    } else {
      imagetype = "jpg"
    }
  } else if (class(image) == "array"){
    temp_image = aperm(image,c(2,1,3))
    imagetype = "none"
  } else {
    stop("`image` not recognized class.")
  }
  if(is.character(depthmap)) {
    if(substring(depthmap, nchar(depthmap)-3,nchar(depthmap)) == ".png") {
      depthtype = "png"
    } else {
      depthtype = "jpg"
    }
  } else if (class(depthmap) == "matrix") {
    depthtype = "none"
  } else {
    stop("`depthmap` not recognized class.")
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
  flipud = function(x) {
    x[nrow(x):1,]
  }
  fliplr = function(x) {
    x[,ncol(x):1]
  }
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

  depthmap[depthmap < mindistance] = mindistance

  if(length(dim(depthmap)) == 3) {
    depthmap = depthmap[,,1]
  }

  depthmap = fliplr(depthmap)
  if(gamma_correction) {
    temp_image = temp_image^2.2
  }
  depthmap2 = calc_bokeh_size(depthmap,focus,focallength, fstop)
  for(i in 1:3) {
    temp_image[,,i] = flipud(t(rayfocus:::psf(t(flipud(temp_image[,,i])), depthmap2, depthmap, focus,
                                              type=bokehshape, custombokeh = custombokeh,
                                              bokehintensity = bokehintensity, bokehlimit=0.8,
                                              rotation=rotation, progbar = progress, channel = i)))
  }
  if(gamma_correction) {
    temp_image = temp_image ^ (1/2.2)
  }
  temp_image[temp_image > 1] = 1
  temp_image[temp_image < 0] = 0
  if(is.null(filename)) {
    rayshader::plot_map(aperm(temp_image,c(2,1,3)))
  } else {
    rayshader::save_png(aperm(temp_image,c(2,1,3)),filename)
  }
}
