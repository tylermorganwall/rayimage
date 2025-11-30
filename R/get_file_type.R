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
    ext = tolower(tools::file_ext(file))
    if (ext == "png") {
      imagetype = "png"
    } else if (ext %in% c("tif", "tiff")) {
      imagetype = "tif"
    } else if (ext %in% c("jpeg", "jpg")) {
      imagetype = "jpg"
    } else if (ext == "exr") {
      imagetype = "exr"
    } else if (ext == "hdr") {
      imagetype = "hdr"
    } else if (ext == "tga") {
      imagetype = "tga"
    } else if (ext == "bmp") {
      imagetype = "bmp"
    } else if (ext == "psd") {
      imagetype = "psd"
    } else if (ext == "gif") {
      imagetype = "gif"
    } else if (ext == "pic") {
      imagetype = "pic"
    } else if (ext %in% c("pnm", "ppm", "pgm")) {
      imagetype = "pnm"
    } else {
      stop(
        "`",
        file,
        "` not recognized class (png, tiff, jpeg, exr, hdr, tga, bmp, psd, gif, pic, pnm, array, matrix)."
      )
    }
  } else if (length(dim(file)) == 3 && dim(file)[3] == 1) {
    imagetype = "matrix"
  } else if (length(dim(file)) == 3) {
    imagetype = "array"
  } else if (length(dim(file)) == 2) {
    imagetype = "matrix"
  } else if (length(dim(file)) > 3) {
    stop(
      "Images can only be paths to image files, matrices, ",
      "and arrays with fewer than 3 dimensions"
    )
  } else {
    if (is.character(file)) {
      stop(
        "`",
        file,
        "` not recognized class (png, tiff, jpeg, exr, hdr, tga, bmp, psd, gif, pic, pnm, array, matrix)."
      )
    } else {
      stop(
        "input not recognized class (png, tiff, jpeg, exr, hdr, tga, bmp, psd, gif, pic, pnm, array, matrix)."
      )
    }
  }
  imagetype
}
