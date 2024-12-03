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
                     title_font = "Arial", title_style = "plain",
                     title_bar_color = NA, title_bar_alpha = 0.5, title_bar_width = NULL,
                     title_position = NA, 
                     use_magick = FALSE,
                     filename = NULL, preview = FALSE) {
  imagetype = get_file_type(image)
  temp = tempfile(fileext = ".png")
  ray_write_image(image, temp)
  temp_image = png::readPNG(temp)

  if(use_magick) {
    if(title_style == "plain") {
      title_style = "normal"
    }
    if(is.na(title_bar_color)) {
      title_bar_color = NULL
    }
    if(is.na(title_position)) {
      title_position = "northwest"
    }
    dimensions = dim(temp_image)
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

  } else {
    get_font_metrics = function(font, font_size, style = "plain") {
      # Create a temporary file to write and read the images
      temp_ascender = tempfile(fileext = ".png")
      temp_descender = tempfile(fileext = ".png")
      temp_neutral = tempfile(fileext = ".png")
      
      # Characters for bounding box measurement
      char_ascender = "d" # Tall character
      char_descender = "g" # Descending character
      char_neutral = "x" # Neutral character
      
      # Function to render a character and return dimensions
      render_character = function(char, output_file) {
        grDevices::png(output_file, width = 100, height = 100, bg = "transparent")
        grid::grid.newpage()
        grid::grid.text(char, x = 0.5, y = 0.5, 
                        gp = grid::gpar(fontfamily = font, fontsize = font_size, fontface = style))
        dev.off()
        img = png::readPNG(output_file)
        bounding_box = which(apply(img,1,sum) != 0)
        dimensions = list(
          top = min(bounding_box),
          bottom = max(bounding_box),
          height = max(bounding_box) - min(bounding_box) + 1
        )
        return(dimensions)
      }
      
      # Render each character and compute dimensions
      metrics_ascender = render_character(char_ascender, temp_ascender)
      metrics_descender = render_character(char_descender, temp_descender)
      metrics_neutral = render_character(char_neutral, temp_neutral)
      
      # Calculate adjustments
      ascender_height = metrics_ascender$height
      descender_height = metrics_descender$height
      neutral_height = metrics_neutral$height
      
      ascender_adjustment = ascender_height - neutral_height
      descender_adjustment = descender_height - neutral_height
      
      # Return metrics
      list(
        ascender_adjustment = ascender_adjustment,
        descender_adjustment = descender_adjustment,
        neutral_height = neutral_height
      )
    }
    font_metrics  = get_font_metrics(title_font, title_size, title_style)
    draw_title_card = function(
      image,
      title,
      padding_x = 10, padding_y = 10,
      gp_text = grid::gpar(col = "white", fontsize = 12),
      line_spacing = 0.5,
      bg_color = "black", bg_alpha = 0.65) {
      grDevices::png(temp,
        width = ncol(image),
        height = nrow(image),
        pointsize = gp_text$fontsize,
      )
      grid::grid.newpage()
      plot_image(image, gp = gp_text)
      grid::seekViewport("image")
    
      # Get image dimensions
      vp = grid::current.viewport()
    
      image_width = diff(vp$xscale)
      image_height = abs(diff(vp$yscale))
    
      # Available width for text
      available_width = image_width - padding_x * 2
    
      # Wrap text into lines based on available width
      wrap_text = function(text, available_width) {
        words = strsplit(text, "\\s+")[[1]]
        lines = character(0)
        current_line = words[1]
    
        for (word in words[-1]) {
          test_line = paste(current_line, word)
          test_width = grid::convertWidth(grid::stringWidth(test_line), "native", valueOnly = TRUE)
          if (test_width > available_width) {
            lines = c(lines, current_line)
            current_line = word
          } else {
            current_line = test_line
          }
        }
        lines = c(lines, current_line)
        return(lines)
      }
    
      # Wrap the title text
      lines = wrap_text(title, available_width)
      n_lines = length(lines)
      # Calculate text dimensions
      line_heights = sapply(lines, function(line) {
        abs(grid::convertHeight(grid::stringHeight(line), "native", valueOnly = TRUE))
      })
      single_line_height = abs(grid::convertHeight(grid::unit(line_spacing, "strheight", data = ""), 
                               "native", valueOnly = TRUE))
      
      text_height = sum(line_heights) + n_lines * single_line_height
    
      # Position adjustments
      x = padding_x
      y = 0 # Start from top (y increases downward)
      # Draw background rectangle
      descend_adj = font_metrics$descender_adjustment
      grid::grid.rect(
        x = 0, y = y,
        width = image_width, height = text_height + padding_y * 2 - descend_adj ,
        just = c("left", "bottom"),
        default.units = "native",
        gp = grid::gpar(fill = grDevices::adjustcolor(bg_color, alpha.f = bg_alpha), col = NA)
      )
    
      # Draw each line of text
      current_y = padding_y
      for (i in seq_along(lines)) {
        current_y = current_y
        grid::grid.text(
          lines[i],
          x = x, y = current_y,
          just = c("left", "top"),
          default.units = "native",
          gp = gp_text
        )
        current_y = current_y + line_heights[i] + single_line_height
      }
      dev.off()
    }
    if(!is.na(title_position)) {
      warning("Title position is ignored when not using {magick}")
    }
    draw_title_card(
      temp_image,
      title = title_text,
      padding_x = title_offset[1], padding_y = title_offset[2],
      gp_text = grid::gpar(col = title_color, fontsize = title_size,
        fontfamily = title_font, 
        fontface = title_style, fill = NA),
      line_spacing = 0.5,
      bg_color = title_bar_color, bg_alpha = title_bar_alpha)
  }
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
