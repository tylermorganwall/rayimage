#' Check Filename
#'
#' @param file Filename to be checked
#' @return Flipped matrix
#' @keywords internal
#'
#' @examples
#' #Fake example
get_file_type = function(file) {
  if(is.character(file)) {
    if(tools::file_ext(file) == "png") {
      imagetype = "png"
    } else if (tools::file_ext(file) %in% c("tif", "tiff")) {
      imagetype = "tif"
    } else if (tools::file_ext(file) %in% c("jpeg", "jpg")) {
      imagetype = "jpg"
    } else {
      stop("`", file,"` not recognized class (png, tiff, jpeg, array, matrix).")
    }
  } else if (length(dim(file)) == 3) {
    imagetype = "array"
  } else if (length(dim(file)) == 2) {
    imagetype = "matrix"
  } else {
    stop("`",file,"` not recognized class (png, tiff, jpeg, array, matrix).")
  }
}
