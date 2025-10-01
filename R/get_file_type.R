#' Check Filename
#'
#' @param file Filename to be checked
#' @return Filename
#' @keywords internal
#'
#' @examples
#' #Fake example
get_file_type = function(file) {
  if (is.character(file)) {
    if (tools::file_ext(file) == "png") {
      imagetype = "png"
    } else if (tools::file_ext(file) %in% c("tif", "tiff")) {
      imagetype = "tif"
    } else if (tools::file_ext(file) %in% c("jpeg", "jpg")) {
      imagetype = "jpg"
    } else if (tools::file_ext(file) == "exr") {
      imagetype = "exr"
    } else {
      stop(
        "`",
        file,
        "` not recognized class (png, tiff, jpeg, exr, array, matrix)."
      )
    }
  } else if (length(dim(file)) == 3 && dim(file)[3] == 1) {
    imagetype = "matrix"
  } else if (length(dim(file)) == 3) {
    imagetype = "array"
  } else if (length(dim(file)) == 2) {
    imagetype = "matrix"
  } else if (length(dim(file)) > 3) {
    stop("Images can only be paths to image files, matrices, ",
				 "and arrays with fewer than 3 dimensions")
  } else {
	if(is.character(file)) {
			stop(
				"`",
				file,
				"` not recognized class (png, tiff, jpeg, exr, array, matrix)."
			)
		} else {
	  	stop("input not recognized class (png, tiff, jpeg, exr, array, matrix).")
		}
	} 
}
