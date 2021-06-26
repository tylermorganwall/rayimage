#'@title Render Boolean Distance
#'
#'@description Takes an matrix (or  and returns the nearest distance to each TRUE.
#'
#'@param boolean Logical matrix (or matrix of 1s and 0s), where distance will be measured to the `TRUE` values.
#'@param rescale Default `FALSE`. Rescales the calculated distance to a range of 0-1.
#'Useful for visualizing the distance matrix.
#'@return Matrix of distance values.
#'@export
#'@examples
#'#if(interactive()){
#'#Measure distance to
#'image(render_boolean_distance(volcano > 150))
#'image(render_boolean_distance(volcano < 150))
#'
#'#If we want to rescale this to zero to one (to visualize like an image), set rescale=TRUE
#'plot_image(render_boolean_distance(volcano > 150,rescale=TRUE))
#'#end}
render_boolean_distance = function(boolean, rescale = FALSE) {
  if(any(is.na(boolean))) {
    stop("`boolean` must not have any NA values")
  }
  distval = get_boolean_distance(boolean)
  if(rescale) {
    if(min(distval) != max(distval)) {
      distval = (distval-min(distval))/(max(distval) - min(distval))
    }
    return(distval)
  } else {
    return(distval)
  }
}
