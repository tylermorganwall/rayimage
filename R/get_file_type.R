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
    } else {
      imagetype = "jpg"
    }
  } else if (inherits(file,"array")) {
    imagetype = "array"
  } else if (inherits(file,"matrix")) {
    imagetype = "matrix"
  } else {
    stop("`",file,"` not recognized class (png, jpeg, array, matrix).")
  }

}
