#'@title Resize Image
#'
#'@description Resizes an image or a matrix, using bilinear interpolation.
#'
#'@param image Image filename, 3-layer RGB array, or matrix.
#'@param mag Default `1`. Amount to magnify the image, preserving aspect ratio. Overridden if
#'`dim` is not `NULL`.
#'@param dims Default `NULL`. Exact resized dimensions.
#'@param filename Default `NULL`. The filename of the image to be saved. If this is not given, the image will be plotted instead.
#'@param preview Default `FALSE`. Whether to plot the convolved image, or just to return the values.
#'@return 3-layer RGB resized array or matrix.
#'@export
#'@examples
#'#Plot the image with a title
#'\donttest{
#'dragon %>%
#'  add_title("Dragon", title_offset=c(10,10), title_bar_color="black",
#'            title_size=20, title_color = "white") %>%
#'  plot_image()
#'}
#'
#'#Half of the resolution
#'\donttest{
#'render_resized(dragon, mag = 1/2) %>%
#'  add_title("Dragon (half res)", title_offset=c(5,5), title_bar_color="black",
#'            title_size=10, title_color = "white") %>%
#'  plot_image()
#'}
#'#Double the resolution
#'\donttest{
#'render_resized(dragon, mag = 2) %>%
#'  add_title("Dragon (2x res)", title_offset=c(20,20), title_bar_color="black",
#'            title_size=40, title_color = "white") %>%
#'  plot_image()
#'}
#'#Specify the exact resulting dimensions
#'\donttest{
#'render_resized(dragon, dim = c(320,160)) %>%
#'  add_title("Dragon (custom size)", title_offset=c(10,10), title_bar_color="black",
#'            title_size=20, title_color = "white") %>%
#'  plot_image()
#'}
render_resized = function(image, mag = 1, dims = NULL, filename=NULL, preview=FALSE) {
  if(!is.null(filename)) {
    if(tools::file_ext(filename) != "png") {
      filename = paste0(filename,".png")
    }
  }
  imagetype = get_file_type(image)
  if(imagetype == "array") {
    temp_image = aperm(image,c(2,1,3))
  } else if(imagetype == "jpg") {
    temp_image = suppressWarnings(aperm(jpeg::readJPEG(image),c(2,1,3)))
  } else if (imagetype == "png"){
    temp_image = suppressWarnings(aperm(png::readPNG(image),c(2,1,3)))
  } else if (imagetype == "matrix") {
    temp_image = t(image)
  }
  if(!is.null(dims)) {
    dims = dims/dim(temp_image)[1:2]
  }
  temp_list = list()
  if(imagetype != "matrix") {
    for(i in 1:3) {
      if(is.null(dims)) {
        temp_list[[i]] = resize_image(t(flipud(temp_image[,,i])), mag)
      } else {
        temp_list[[i]] = resize_image_xy(t(flipud(temp_image[,,i])), dims[1], dims[2])
      }
    }
    temp_image = array(0, dim = c(nrow(temp_list[[1]]), ncol(temp_list[[1]]), 3))
    temp_image[,,1] = temp_list[[1]]
    temp_image[,,2] = temp_list[[2]]
    temp_image[,,3] = temp_list[[3]]
  } else {
    if(is.null(dims)) {
      temp_image = resize_image(t(flipud(temp_image[,,i])), mag)
    } else {
      temp_image = resize_image_xy(t(flipud(temp_image[,,i])), dims[1], dims[2])
    }
  }
  if(is.null(filename)) {
    if(preview) {
      temp_image[temp_image > 1] = 1
      temp_image[temp_image < 0] = 0
      plot_image(fliplr(temp_image))
    } else {
      fliplr(temp_image)
    }
  } else {
    temp_image[temp_image > 1] = 1
    temp_image[temp_image < 0] = 0
    save_png(fliplr(temp_image),filename)
  }
}
