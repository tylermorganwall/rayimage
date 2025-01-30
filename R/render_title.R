#' @title Render a Title on an Image
#'
#' @description Adds a title with optional styling and a title bar to an image.
#' The image can be previewed or saved to a file. Supports both the `grid`-based
#' method and (deprecated) `magick` package for rendering the title.
#'
#' @param image Image filename or 3-layer RGB array. Specifies the image to process.
#' @param title_text Default `""`. Text string to be added as the title to the image.
#' @param title_offset Default `c(15,15)`. Numeric vector specifying the horizontal
#' and vertical offset of the title text, relative to its anchor position.
#' @param title_lineheight Default `1`. Multiplier for the lineheight.
#' @param title_color Default `"black"`. String specifying the color of the title text.
#' @param title_size Default `30`. Numeric value specifying the font size of the title text.
#' @param title_font Default `"Arial"`. String specifying the font family for the title text.
#' Common options include `"sans"`, `"mono"`, `"serif"`, `"Times"`, `"Helvetica"`, etc.
#' @param title_style Default `"plain"`. String specifying the font style, such as
#' `"plain"`, `"italic"`, or `"bold"`.
#' @param title_bar_color Default `NULL`. Color of the optional title bar. If `NULL`, no bar is added.
#' @param title_bar_alpha Default `0.5`. Transparency level of the title bar. A value
#' between `0` (fully transparent) and `1` (fully opaque).
#' @param title_bar_width Default `NULL`. Numeric value for the height of the title bar in pixels.
#' If `NULL`, it is automatically calculated based on the text size and line breaks.
#' @param title_position Default `"northwest"`. String specifying the position of the title text.
#' Only used when `use_magick = TRUE`. Common options include `"northwest"`, `"center"`, `"south"`, etc.
#' @param title_just Default `"left"`. Horizontal alignment of the title text: `"left"`,
#' `"center"`, or `"right"`.
#' @param use_magick Default `FALSE`. Boolean indicating whether to use the `magick` package for
#' rendering titles. This option will be deprecated in future versions.
#' @param filename Default `NULL`. String specifying the file path to save the resulting image.
#' If `NULL` and `preview = FALSE`, the function returns the processed RGB array.
#' @param preview Default `FALSE`. Boolean indicating whether to display the image after processing.
#' If `TRUE`, the image is displayed but not saved or returned.
#'
#' @return A 3-layer RGB array of the processed image if `filename = NULL` and `preview = FALSE`.
#' Otherwise, writes the image to the specified file or displays it if `preview = TRUE`.
#'
#' @note The `use_magick` parameter and all functionality tied to the `magick` package are
#' planned for deprecation. It is recommended to use the `grid`-based method for
#' future compatibility.
#'
#' @import grDevices grid
#' @export
#'
#' @examples
#'if(run_documentation()){
#'#Plot the dragon
#'render_title(dragon, preview = TRUE, title_text = "Dragon", title_size=20)
#'}
#'if(run_documentation()){
#'#That's hard to see--let's add a title bar:
#'render_title(dragon, preview = TRUE, title_text = "Dragon", title_size=20,
#'          title_bar_color="white")
#'}
#'if(run_documentation()){
#'#Change the width of the bar:
#'render_title(dragon, preview = TRUE, title_text = "Dragon", title_size=20,
#'          title_bar_color="white", title_offset = c(8,8))
#'}
#'if(run_documentation()){
#'#The width of the bar will also automatically adjust for newlines:
#'render_title(dragon, preview = TRUE, title_text = "Dragon\n(Blue)", title_size=20,
#'          title_bar_color="white")
#'}
#'if(run_documentation()){
#'#Change the color and title color:
#'render_title(dragon, preview = TRUE, title_text = "Dragon", title_size=20,
#'          title_bar_color="red", title_color = "white")
#'}
#'if(run_documentation()){
#'#Change the transparency:
#'render_title(dragon, preview = TRUE, title_text = "Dragon",
#'             title_size=20, title_bar_alpha = 0.8,
#'             title_bar_color="red", title_color = "white")
#'}
#'if(run_documentation()){
#' #Read directly from a file
#' temp_image = tempfile(fileext = ".png")
#' ray_write_image(dragon, temp_image)
#' render_title(temp_image, preview = TRUE, title_text = "Dragon",
#'              title_size=20, title_bar_alpha = 0.8,
#'              title_bar_color="red", title_color = "white")
#'}
render_title = function(image,
                     title_text = "", title_size = 30,
                     title_offset = rep(title_size/2,2),
                     title_lineheight = 1,
                     title_color = "black",
                     title_font = "Arial", title_style = "plain",
                     title_bar_color = NA, title_bar_alpha = 0.5, title_bar_width = NULL,
                     title_position = NA, title_just = "left",
                     use_magick = FALSE,
                     filename = NULL, preview = FALSE) {
  imagetype = get_file_type(image)
  temp = tempfile(fileext = ".png")
  if(imagetype %in% c("matrix", "array")) {
    ray_write_image(image, temp)
    temp_image = png::readPNG(temp)
  } else {
    temp_image = image
  }

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
    draw_title_card = function(
      image,
      title,
      padding_x = 10, padding_y = 10,
      gp_text = grid::gpar(col = "white", fontsize = title_size),
      bg_color = "black", bg_alpha = 0.65,
      title_just = c("left", "top")) {
      grDevices::png(temp,
        width = ncol(image),
        height = nrow(image),
        pointsize = 12,
        family = title_font,
        res = 72
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
        # Split text into segments by newline characters
        segments = unlist(strsplit(text, "\n"))
        all_lines = character(0)

        for (segment in segments) {
          words = strsplit(segment, "\\s+")[[1]]
          if (length(words) == 0) {
            all_lines = c(all_lines, "")  # Add an empty line for consecutive newlines
            next
          }

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
          all_lines = c(all_lines, lines)
        }

        return(all_lines)
      }

      # Wrap the title text
      lines = wrap_text(title, available_width)
      lines_with_newlines = paste0(lines, collapse="\n")

      text_size = systemfonts::shape_string(lines_with_newlines,
        size = title_size, vjust = 0,
        res=72, lineheight = title_lineheight,
        family = title_font)[["metrics"]]
      # Position adjustments
      if (!title_just[1] %in% c('left', 'center', 'right')) {
        stop('Invalid title_just value for horizontal alignment. Must be "left", "center", or "right".')
      }
      if (title_just[1] == 'left') {
        x = padding_x
        just = c('left', 'top')
      } else if (title_just[1] == 'center') {
        x = image_width / 2
        padding_x = 0
        just = c('center', 'top')
      } else if (title_just[1] == 'right') {
        x = image_width - padding_x
        just = c('right', 'top')
      }
      height_with_padding = text_size$height -
        text_size$top_bearing -
        text_size$bottom_bearing -
        text_size$top_border +
        padding_y*2
      # Draw background rectangle
      grid::grid.rect(
        x = 0, y = 0,
        width = image_width,
        height = height_with_padding,
        just = c("left", "bottom"),
        default.units = "native",
        gp = grid::gpar(fill =
          grDevices::adjustcolor(bg_color, alpha.f = bg_alpha), col = NA)
      )
      grid::grid.text(label = lines_with_newlines,
        x = padding_x, y = padding_y,
        default.units = "native",
        just = c("left","top"),
        gp = grid::gpar(
          fontsize = title_size,
          lineheight = title_lineheight,
          cex = 1,
          fontfamily = title_font)
      )
      dev.off()
    }
    if(!is.na(title_position)) {
      warning("Title position is ignored when not using {magick}")
    }
    if(is.null(title_bar_color)) {
      title_bar_color = NA
    }
    if(is.null(title_bar_alpha)) {
      title_bar_alpha = NA
    }
    draw_title_card(
      ray_read_image(temp_image),
      title = title_text,
      padding_x = title_offset[1], padding_y = title_offset[2],
      gp_text = grid::gpar(col = title_color,
        fontsize = title_size,
        fontfamily = title_font,
        lineheight = title_lineheight,
        fontface = title_style, fill = NA),
      bg_color = title_bar_color, bg_alpha = title_bar_alpha,
      title_just = title_just)
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
  handle_image_output(temp, filename = filename, preview = preview)
}

#' Add Title Function (deprecated)
#'
#' @param ... Arguments to pass to `render_title()` function.
#'
#' @return A 3-layer RGB array of the processed image if `filename = NULL` and `preview = FALSE`.
#' Otherwise, writes the image to the specified file or displays it if `preview = TRUE`.
#' @export
#' @import grDevices grid
#'
#' @examples
#' #Deprecated add_title function. Will display a warning.
#' if(run_documentation()) {
#' render_title(dragon, preview = TRUE, title_text = "Dragon", title_size=20)
#' }
add_title = function(...) {
  message("add_title() deprecated--use render_title() instead.")
  render_title(...)
}

