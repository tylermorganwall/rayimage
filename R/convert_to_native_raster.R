#'@title Plot Image
#'
#'@description Displays the image in the current device.
#'
#'@param input Array to get converted to a native raster
#'@keywords internal
convert_to_native_raster = function(input) {
  ncol = ncol(input)
  nrow = nrow(input)
  if(length(dim(input)) == 3) {
    if(dim(input)[3] == 3) {
      channels = 3
      nr = encode_native_image_rcpp_3(input[,,1],input[,,2],input[,,3])
    } else if (dim(input)[3] == 4){
      channels = 4
      nr = encode_native_image_rcpp_4(input[,,1],input[,,2],input[,,3],input[,,4])
    } else if (dim(input)[3] == 2){
      channels = 3
      nr = encode_native_image_rcpp_4(input[,,1],input[,,1],input[,,1],input[,,2])
    } else {
      stop(sprintf("Dimension of array (%d/%d/%d) do not correspond to any image types",
                   dim(input)[1],dim(input)[2],dim(input)[3]))
    }
  } else if (length(dim(input)) == 2) {
    channels = 3
    nr = encode_native_image_rcpp_1(input)
  } else {
    stop(sprintf("Dimensions of array do not correspond to any image types"))
  }
  class(nr) = "nativeRaster"
  dim(nr) = c(nrow,ncol)
  attr(nr,"channels") = channels
  return(nr)
}
