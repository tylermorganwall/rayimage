#'@title Add Title
#'
#'@description Takes an RGB array/filename and adds a title with an optional titlebar.
#'
#'@param image Image filename or 3-layer RGB array.
#'@param filename Default `NULL`. File to save the image to. If `NULL` and `preview = FALSE`,
#'returns an RGB array.
#'@param preview Default `FALSE`. If `TRUE`, it will display the image in addition
#'to returning it.
#'@param title_text Default `NULL`. Text. Adds a title to the image, using magick::image_annotate.
#'@param title_offset Default `c(20,20)`. Distance from the top-left (default, `gravity` direction in
#'image_annotate) corner to offset the title.
#'@param title_size Default `30`. Font size in pixels.
#'@param title_color Default `black`. Font color.
#'@param title_font Default `sans`. String with font family such as "sans", "mono", "serif", "Times", "Helvetica",
#'"Trebuchet", "Georgia", "Palatino" or "Comic Sans".
#'@param title_style Default `normal`. Font style (e.g. `italic`).
#'@param title_bar_color Default `NULL`. If a color, this will create a colored bar under the title.
#'@param title_bar_alpha Default `0.5`. Transparency of the title bar.
#'@param title_position Default `northwest`. Position of the title.
#'@return 3-layer RGB array of the processed image.
#'@import grDevices
#'@export
#'@examples
#'#Plot the dragon
#'add_title(dragon, preview = TRUE, title_text = "Dragon", title_size=20)
#'
#'#That's hard to see--let's add a title bar:
#'\donttest{
#'add_title(dragon, preview = TRUE, title_text = "Dragon", title_size=20,
#'          title_bar_color="white")
#'}
#'
#'#Change the width of the bar:
#'\donttest{
#'add_title(dragon, preview = TRUE, title_text = "Dragon", title_size=20,
#'          title_bar_color="white", title_offset = c(12,12))
#'}
#'#Change the color and title color:
#'\donttest{
#'add_title(dragon, preview = TRUE, title_text = "Dragon", title_size=20,
#'          title_bar_color="red", title_color = "white", title_offset = c(12,12))
#'}
#'
#'#Change the transparency:
#'\donttest{
#'add_title(dragon, preview = TRUE, title_text = "Dragon", title_size=20, title_bar_alpha = 0.8,
#'          title_bar_color="red", title_color = "white", title_offset = c(12,12))
#'}
#'
add_title = function(image,
                     title_text = "", title_offset = c(20,20),
                     title_color = "black", title_size = 30,
                     title_font = "sans", title_style = "normal",
                     title_bar_color = NULL, title_bar_alpha = 0.5, title_position = "northwest",
                     filename = NULL, preview = FALSE) {
  imagetype = get_file_type(image)
  temp = tempfile(fileext = ".png")
  if(imagetype == "array") {
    #Clip HDR images
    image[image > 1] = 1
    png::writePNG(image,temp)
    image = temp
  } else if (imagetype == "matrix") {
    newarray = array(0,dim=c(nrow(image),ncol(image),3))
    newarray[,,1] = image
    newarray[,,2] = image
    newarray[,,3] = image
    #Clip HDR images
    newarray[newarray > 1] = 1
    png::writePNG(newarray,temp)
    image = temp
  } else if (imagetype == "png") {
    image = png::readPNG(image)
    png::writePNG(image,temp)
  } else if (imagetype == "jpg") {
    image = jpeg::readJPEG(image)
    png::writePNG(image,temp)
  }
  tempmap = png::readPNG(temp)
  dimensions = dim(tempmap)

  if(!("magick" %in% rownames(utils::installed.packages()))) {
    stop("`magick` package required for adding title")
  }
  if(!is.null(title_bar_color)) {
    title_bar_color = col2rgb(title_bar_color)/255
    title_bar = array(0,c(dimensions[1],dimensions[2],4))
    title_bar_width = 2 * title_offset[2] + title_size
    if(title_bar_width > dimensions[1]) {
      message(paste0(c("Input title_bar_width (", title_bar_width,
              ") greater than image height (",
              dimensions[1],
              "), reducing size."),collapse=""))
      title_bar_width = dimensions[1]
    }
    title_bar[1:title_bar_width,,1] = title_bar_color[1]
    title_bar[1:title_bar_width,,2] = title_bar_color[2]
    title_bar[1:title_bar_width,,3] = title_bar_color[3]
    title_bar[1:title_bar_width,,4] = title_bar_alpha
    title_bar_temp = paste0(tempfile(),".png")
    png::writePNG(title_bar,title_bar_temp)
    magick::image_read(temp) %>%
      magick::image_composite(magick::image_read(title_bar_temp),
      ) %>%
      magick::image_write(path = temp, format = "png")
  }
  magick::image_read(temp) %>%
    magick::image_annotate(title_text,
                           location = paste0("+", title_offset[1],"+",title_offset[2]),
                           size = title_size, color = title_color, style = title_style,
                           font = title_font, gravity = title_position) %>%
    magick::image_write(path = temp, format = "png")
  temp = png::readPNG(temp)
  if(length(dim(temp)) == 3 && dim(temp)[3] == 2) {
    temparray = array(0,dim = c(nrow(temp),ncol(temp),3))
    temparray[,,1] = temp[,,1]
    temparray[,,2] = temp[,,1]
    temparray[,,3] = temp[,,1]
    temp = temparray
  }
  if(length(dim(temp)) == 2) {
    temparray = array(0,dim = c(nrow(temp),ncol(temp),3))
    temparray[,,1] = temp
    temparray[,,2] = temp
    temparray[,,3] = temp
    temp = temparray
  }
  if(missing(filename)) {
    if(!preview) {
      return(temp)
    }
    plot_image(temp, keep_user_par = FALSE)
    return(invisible(temp))
  } else {
    save_png(temp, filename)
    return(invisible(temp))
  }
}
