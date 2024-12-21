#'@title Render Bokeh
#'
#'@description Takes an image and a depth map to render the image with depth of field
#'(i.e. similar to "Portrait Mode" in an iPhone). User can specify a custom bokeh shape,
#'or use one of the built-in bokeh types.
#'
#'@param image Image filename or 3-layer RGB array.
#'@param depthmap Depth map filename or 1d array.
#'@param focus Defaults `0.5`. Depth in which to blur.
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
#'@param ... Additional arguments to pass to `plot_image()` if `preview = TRUE`.
#'@return 3-layer RGB array of the processed image.
#'@export
#'@examples
#'if(run_documentation()){
#'#Plot the dragon
#'plot_image(dragon)
#'}
#'if(run_documentation()){
#'#Plot the depth map
#' plot_image(dragondepth/1500)
#'}
#'if(run_documentation()){
#'#Preview the focal plane:
#'render_bokeh(dragon, dragondepth, focus=950, preview_focus = TRUE)
#'}
#'if(run_documentation()){
#'#Change the focal length:
#'render_bokeh(dragon, dragondepth, focus=950, focallength=300)
#'}
#'if(run_documentation()){
#'#Add chromatic aberration:
#'render_bokeh(dragon, dragondepth, focus=950, focallength=300, aberration = 0.5)
#'}
#'if(run_documentation()){
#'#Change the focal distance:
#'render_bokeh(dragon, dragondepth, focus=600, focallength=300)
#'render_bokeh(dragon, dragondepth, focus=1300, focallength=300)
#'}
#'if(run_documentation()){
#'#Change the bokeh shape to a hexagon:
#'render_bokeh(dragon, dragondepth, bokehshape = "hex",
#'             focallength=300, focus=600)
#'}
#'if(run_documentation()){
#'#Change the bokeh intensity:
#'render_bokeh(dragon, dragondepth,
#'             focallength=400, focus=900, bokehintensity = 1)
#'render_bokeh(dragon, dragondepth,
#'             focallength=400, focus=900, bokehintensity = 3)
#'}
#'if(run_documentation()){
#'#Rotate the hexagonal shape:
#'render_bokeh(dragon, dragondepth, bokehshape = "hex", rotation=15,
#'             focallength=300, focus=600)
#'}
render_bokeh = function(image, depthmap,
                        focus=0.5, focallength = 100, fstop = 4, filename=NULL,
                        preview = TRUE, preview_focus = FALSE,
                        bokehshape = "circle", bokehintensity = 1, bokehlimit = 0.8, rotation=0,
                        aberration = 0, gamma_correction = TRUE, progress = interactive(),
                        ...) {
  imagetype = get_file_type(image)
  temp_image = ray_read_image(image)

  depthtype = get_file_type(depthmap)
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

  depthmap = ray_read_image(depthmap, convert_to_array = FALSE)

  if(length(dim(depthmap)) == 3) {
    depthmap = depthmap[,,1]
  }

  depthmap[is.na(depthmap)] = max(depthmap, na.rm = TRUE)*2
  # depthmap = t((depthmap))

  if(gamma_correction) {
    temp_image = temp_image^2.2
  }
  for(i in 1:3) {
    max_size = min(c(max(dim(depthmap)),500))
    if(i == 1) {
      depthmap2 = calc_bokeh_size(depthmap,focus,focallength, fstop,1+aberration)
    } else if(i ==2) {
      depthmap2 = calc_bokeh_size(depthmap,focus,focallength, fstop,1)
    } else {
      depthmap2 = calc_bokeh_size(depthmap,focus,focallength, fstop,1-aberration)
    }
    if(any(dim(depthmap2)[1:2] != dim(temp_image)[1:2])) {
      stop(sprintf("{rayimage}: dimensions of image (%ix%i) and depth map (%ix%i) don't match",
                   dim(temp_image)[1],dim(temp_image)[2],
                   dim(depthmap2)[1], dim(depthmap2)[2]))
    }
    depthmap2[depthmap2 > max_size] = max_size
    temp_image[,,i] = psf(temp_image[,,i],depthmap2,
                          depthmap, focus, bokehshape, custombokeh = custombokeh,
                          bokehintensity, bokehlimit, rotation, progbar = progress,channel = i)
  }
  if(gamma_correction) {
    temp_image = temp_image ^ (1/2.2)
  }
  handle_image_output(temp_image, filename = filename, preview = preview)
}
