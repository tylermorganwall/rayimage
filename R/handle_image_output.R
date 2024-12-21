#' @title Handle Image Output
#'
#' @description Handles image output logic: returning, saving to a file, or previewing.
#'
#' @param image 3D array. The image data.
#' @param filename Default `NULL`. String specifying the file path to save the image. 
#' If `NULL`, the image is returned or displayed.
#' @param preview Default `FALSE`. If `TRUE`, the image is displayed.
#'
#' @return If `filename = NULL` and `preview = FALSE`, returns the image array invisibly.
#' Otherwise, saves to file or displays the image.
#'
#' @keywords internal
handle_image_output = function(image, filename = NULL, preview = FALSE) {
  if (is.null(filename)) {
    if (!preview) {
      return(image) # Return the image data
    }
    plot_image(render_clamp(image)) # Display image
    return(invisible(image))
  } else {
    ray_write_image(image, filename) # Save the image to file
    if (preview) {
      plot_image(render_clamp(image)) # Display saved image
    }
    return(invisible(image))
  }
}