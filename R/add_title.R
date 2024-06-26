#'@title Add Title
#'
#'@description Takes an RGB array/filename and adds a title with an optional titlebar.
#'
#'@param image Image filename or 3-layer RGB array.
#'@param filename Default `NULL`. File to save the image to. If `NULL` and `preview = FALSE`,
#'returns an RGB array.
#'@param preview Default `FALSE`. If `TRUE`, it will display the image in addition
#'to returning it.
#'@param title_text Default `NULL`. Text. Adds a title to the image, using `magick::image_annotate()`.
#'@param title_offset Default `c(15,15)`. Distance from the top-left (default, `gravity` direction in
#'image_annotate) corner to offset the title.
#'@param title_size Default `30`. Font size in pixels.
#'@param title_color Default `black`. Font color.
#'@param title_font Default `sans`. String with font family such as "sans", "mono", "serif", "Times", "Helvetica",
#'"Trebuchet", "Georgia", "Palatino" or "Comic Sans".
#'@param title_style Default `normal`. Font style (e.g. `italic`).
#'@param title_bar_color Default `NULL`. If a color, this will create a colored bar under the title.
#'@param title_bar_alpha Default `0.5`. Transparency of the title bar.
#'@param title_bar_width Default `NULL`, automaticly calculated from the size of the text and
#'the number of line breaks. Width of the title bar in pixels.
#'@param title_position Default `northwest`. Position of the title.
#'@return 3-layer RGB array of the processed image.
#'@import grDevices
#'@export
#'@examples
#'if(run_documentation()){
#'#Plot the dragon
#'add_title(dragon, preview = TRUE, title_text = "Dragon", title_size=20)
#'}
#'if(run_documentation()){
#'#That's hard to see--let's add a title bar:
#'add_title(dragon, preview = TRUE, title_text = "Dragon", title_size=20,
#'          title_bar_color="white")
#'}
#'if(run_documentation()){
#'#Change the width of the bar:
#'add_title(dragon, preview = TRUE, title_text = "Dragon", title_size=20,
#'          title_bar_color="white", title_offset = c(8,8))
#'}
#'if(run_documentation()){
#'#The width of the bar will also automatically adjust for newlines:
#'add_title(dragon, preview = TRUE, title_text = "Dragon\n(Blue)", title_size=20,
#'          title_bar_color="white")
#'}
#'if(run_documentation()){
#'#Change the color and title color:
#'add_title(dragon, preview = TRUE, title_text = "Dragon", title_size=20,
#'          title_bar_color="red", title_color = "white")
#'}
#'if(run_documentation()){
#'#Change the transparency:
#'add_title(dragon, preview = TRUE, title_text = "Dragon", title_size=20, title_bar_alpha = 0.8,
#'          title_bar_color="red", title_color = "white")
#'}
add_title = function(image,
                     title_text = "", title_offset = c(15,15),
                     title_color = "black", title_size = 30,
                     title_font = "sans", title_style = "normal",
                     title_bar_color = NULL, title_bar_alpha = 0.5, title_bar_width = NULL,
                     title_position = "northwest",
                     filename = NULL, preview = FALSE) {
  imagetype = get_file_type(image)
  temp = tempfile(fileext = ".png")
  ray_write_image(image, temp)

  tempmap = png::readPNG(temp)
  dimensions = dim(tempmap)

  if(!("magick" %in% rownames(utils::installed.packages()))) {
    stop("`magick` package required for adding title")
  }

  if(!is.null(title_bar_color)) {
    title_bar_color = col2rgb(title_bar_color)/255
    title_bar = array(0,c(dimensions[1],dimensions[2],4))
    if(is.null(title_bar_width)) {
      #Detect newlines and adjust title bar width
      char_vector = unlist(strsplit(title_text,""))
      newline_indices = which(char_vector == "\n")
      newline_indices = newline_indices[newline_indices != length(char_vector)]
      newlines = length(newline_indices)
      title_bar_width = 2 * title_offset[2] + title_size * (1 + newlines)
    }
    if(title_bar_width > dimensions[1]) {
      message(paste0(c("Input title_bar_width (", title_bar_width,
              ") greater than image height (",
              dimensions[1],
              "), reducing size."),collapse=""))
      title_bar_width = dimensions[1]
    }
    if(title_position %in% c("northwest","north","northeast")) {
      title_bar[1:title_bar_width,,1] = title_bar_color[1]
      title_bar[1:title_bar_width,,2] = title_bar_color[2]
      title_bar[1:title_bar_width,,3] = title_bar_color[3]
      title_bar[1:title_bar_width,,4] = title_bar_alpha
    } else if (title_position %in% c("southwest","south","southeast")) {
      title_bar[(nrow(title_bar)-title_bar_width):nrow(title_bar),,1] = title_bar_color[1]
      title_bar[(nrow(title_bar)-title_bar_width):nrow(title_bar),,2] = title_bar_color[2]
      title_bar[(nrow(title_bar)-title_bar_width):nrow(title_bar),,3] = title_bar_color[3]
      title_bar[(nrow(title_bar)-title_bar_width):nrow(title_bar),,4] = title_bar_alpha
    } else {
      title_bar[(nrow(title_bar)/2-title_bar_width/2):(nrow(title_bar)/2+title_bar_width/2),,1] = title_bar_color[1]
      title_bar[(nrow(title_bar)/2-title_bar_width/2):(nrow(title_bar)/2+title_bar_width/2),,2] = title_bar_color[2]
      title_bar[(nrow(title_bar)/2-title_bar_width/2):(nrow(title_bar)/2+title_bar_width/2),,3] = title_bar_color[3]
      title_bar[(nrow(title_bar)/2-title_bar_width/2):(nrow(title_bar)/2+title_bar_width/2),,4] = title_bar_alpha
    }
    title_bar_temp = paste0(tempfile(),".png")
    png::writePNG(title_bar,title_bar_temp)
    magick::image_read(temp) |>
      magick::image_composite(magick::image_read(title_bar_temp),
      ) |>
      magick::image_write(path = temp, format = "png")
  }
  magick::image_read(temp) |>
    magick::image_annotate(title_text,
                           location = paste0("+", title_offset[1],"+",title_offset[2]),
                           size = title_size, color = title_color, style = title_style,
                           font = title_font, gravity = title_position) |>
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
    plot_image(temp)
    return(invisible(temp))
  } else {
    ray_write_image(temp, filename)
    return(invisible(temp))
  }
}
