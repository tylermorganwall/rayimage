#'@title Preview Focus
#'
#'@description Displays the focal point
#'
#'@param image Image filename or 3-layer RGB array.
#'@param depthmap Depth map filename or 1d array.
#'@param focus Defaults `0.5`. Depth in which to blur. Minimum 0, maximum 1.
#'@keywords internal
#'@examples
#'#Plot the dragon
preview_focus = function(image, depthmap, focus, imagetype, depthtype) {
  image = ray_read_image(image)
  depthmap = ray_read_image(depthmap, convert_to_array = FALSE)
  if(length(dim(depthmap)) == 3) {
    depthmap = depthmap[,,1]
  }

  isimage = max(depthmap, na.rm=TRUE) == 1
  if(isimage) {
    maxval = max(depthmap[depthmap != 1],na.rm=TRUE)
  } else {
    maxval = max(depthmap,na.rm=TRUE)
  }
  if(isimage) {
    depthmap[depthmap == 1] = maxval
  }
  depthrange = range(depthmap, na.rm=TRUE)
  range_depth_high = focus + depthrange[2]/200
  range_depth_low  = focus - depthrange[1]/200
  if(range_depth_high >= maxval) {
    range_depth_high = range_depth_high - range(depthmap)[2]/200
  }
  if(any(depthmap < range_depth_high, na.rm=TRUE) & any(depthmap > range_depth_low, na.rm=TRUE)) {
    image[,,1][depthmap < range_depth_high & depthmap > range_depth_low] = 1
    image[,,2][depthmap < range_depth_high & depthmap > range_depth_low] = 0
    image[,,3][depthmap < range_depth_high & depthmap > range_depth_low] = 0
    message(sprintf("Focal range: %g-%g", depthrange[1], depthrange[2]))
    plot_image(image)
  } else {
    message(sprintf("Focus point (%g) not in focal range: %g-%g", focus, depthrange[1],depthrange[2]))
  }
}
