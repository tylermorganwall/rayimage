#'@title Matrix/Array Interpolation
#'
#'@description Given a series of X and Y coordinates and an array/matrix, interpolates the Z coordinate
#'using bilinear interpolation.
#'
#'@param image Image filename, a matrix, or a 3-layer RGB array.
#'@param x X indices (or fractional index) to interpolate.
#'@param y Y indices (or fractional index) to interpolate.
#'
#'@return Either a vector of values (if image is a matrix) or a list of interpolated values
#'from each layer.
#'@export
#'
#'@examples
#'#if(interactive()){
#'#Interpolate a matrix
#'interpolate_array(volcano,c(10,10.1,11),c(30,30.5,33))
#'#Interpolate a 3-layer array (returns list for each channel)
#'interpolate_array(dragon,c(10,10.1,11),c(30,30.5,33))
#'#end}
interpolate_array = function(image, x, y) {
  imagetype = get_file_type(image)
  #Load and rotate images if png
  if(imagetype == "jpg") {
    image = suppressWarnings(aperm(jpeg::readJPEG(image),c(2,1,3)))
  } else if (imagetype == "png"){
    image = suppressWarnings(aperm(png::readPNG(image),c(2,1,3)))
  }
  xy = matrix(c(x,y),nrow=length(x),ncol=2)
  if(length(dim(image)) == 3) {
    output = list()
    output$r = apply(xy,1,(function(x) rayinterp2(image[,,1], x[1],x[2])))
    output$g = apply(xy,1,(function(x) rayinterp2(image[,,2], x[1],x[2])))
    output$b = apply(xy,1,(function(x) rayinterp2(image[,,3], x[1],x[2])))
    return(output)
  } else {
    return(apply(xy,1,(function(x) rayinterp2(image, x[1],x[2]))))
  }
}
