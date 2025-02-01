#' @title Generate Text Image
#'
#' @description Generates an image which tightly fits text.
#'
#' @param text Text to turn into an image.
#' @param color Default `"black"`. String specifying the color of the text.
#' @param size Default `12`. Numeric value specifying the font size of the text.
#' @param font Default `"sans"`. String specifying the font family for the text.
#' Common options include `"sans"`, `"mono"`, `"serif"`, `"Times"`, `"Helvetica"`, etc.
#' @param just Default `"left"`. Horizontal alignment of the text: `"left"`,
#' `"center"`, or `"right"`.
#' @param lineheight Default `1`. Multiplier for the lineheight.
#' @param background_color Default `"white"`. Color of the background.
#' @param background_alpha Default `1`. Transparency of the background. A value
#' between `0` (fully transparent) and `1` (fully opaque).
#' @param use_ragg Default `TRUE`. Whether to use the `ragg` package as the graphics device. Required for emojis.
#' @param width Default `NA`. User-defined textbox width.
#' @param height Default `NA`. User-defined textbox width.
#' @param check_text_width Default `TRUE`. Whether to manually adjust the bounding
#' box of the resulting image to ensure the string bbox is wide enough for the text.
#' Not all systems provide accurate font sizes: this ensures the string is not cut off
#' at the edges, at the cost of needing to repeatedly render the image internally until 
#' a suitable image is found.
#' @param check_text_height Default `FALSE`. Whether to manually adjust the bounding
#' box of the resulting image to ensure the string bbox is tall enough for the text.
#' This will ensure a tight vertical bounding box on the text.
#' @param filename Default `NULL`. String specifying the file path to save the resulting image.
#' If `NULL` and `preview = FALSE`, the function returns the processed RGB array.
#' @param preview Default `FALSE`. Boolean indicating whether to display the image after processing.
#' If `TRUE`, the image is displayed but not saved or returned.
#'
#' @return A 3-layer RGB array of the processed image if `filename = NULL` and `preview = FALSE`.
#' Otherwise, writes the image to the specified file or displays it if `preview = TRUE`.
#'
#'
#' @import grDevices grid
#' @export
#'
#' @examples
#' if (run_documentation()) {
#'   #Generate an image of some text
#'   render_text_image("Some text", preview = TRUE)
#' }
#' if (run_documentation()) {
#'   #Change the font size
#'   render_text_image("Some text", size = 100, preview = TRUE)
#' }
#' if (run_documentation()) {
#'   #Change the font color
#'   render_text_image("Some text", size = 100, color="red",preview = TRUE)
#' }
#' if (run_documentation()) {
#'   #Change the background color and transparency
#'   render_text_image("Some text", size = 50, color="purple",
#'                       background_color="purple", background_alpha = 0.5,
#'                       preview = TRUE)
#' }
#' if (run_documentation()) {
#'   # Plot an emoji with the agg device.
#'   render_text_image("\U0001F600\U0001F680", size = 50, color = "purple", use_ragg = TRUE,
#'                     background_alpha = 0,
#'                     preview = TRUE)
#' }
#'
#' if (run_documentation()) {
#'   # Plot an emoji with the agg device and adjust the height and width (which
#'   # is on by default) to be a tight fit.
#'   render_text_image("\U0001F600\U0001F680", size = 50, color = "purple", use_ragg = TRUE,
#'                     background_alpha = 0, check_text_width = TRUE,
#'                     check_text_height = TRUE,
#'                     preview = TRUE)
#' }
render_text_image = function(
    text,
    lineheight = 1,
    color = "black",
    size = 12,
    font = "sans",
    just = "left",
    background_color = "white",
    background_alpha = 1,
    use_ragg = TRUE, width = NA, height = NA,
    filename = NULL,
    check_text_width = TRUE, check_text_height = TRUE,
    preview = FALSE) {
  text_metrics = systemfonts::shape_string(text,
    size = size, vjust = 0.5,
    res = 72, lineheight = lineheight,
    family = font
  )[["metrics"]]
  bg_col = convert_color(background_color, as_hex = TRUE)
  metrics_string = systemfonts::string_metrics_dev(text, unit="inches", size=size) * 72
  max_total_width = ceiling(metrics_string$width) + 2
  max_total_height = ceiling(text_metrics$height)
  n_newlines = sum(strsplit(text, "") == "\n")
  if(!is.na(width)) {
    max_total_width = width
  }
  if(!is.na(height)) {
    max_total_height = height
  }
  final_array = array(1, dim = c(
    max_total_height,max_total_width,
    4
  ))
  temp_filename = tempfile(fileext = ".png")
  ray_write_image(final_array, temp_filename)
  temp_image = png::readPNG(temp_filename)
  image_width = ncol(temp_image)
  image_height = nrow(temp_image)

  test_edges = function(image_width, image_height) {
    if(length(find.package("ragg",quiet=TRUE)) > 0 && use_ragg) {
      ragg::agg_png(temp_filename,
        width = image_width,
        height = image_height,
        units = "px",
        res = 72,
        background = NA
      )
    } else {
      grDevices::png(temp_filename,
        width = image_width,
        height = image_height,
        pointsize = size,
        family = font,
        res = 72,
        bg = NA
      )
    }

    #First write with no background
    grid::grid.newpage()
    grid::grid.rect(
      x = 0.5, y = 0.5,
      width = 1,
      height = 1,
      just = c("center"),
      default.units = "npc",
      gp = grid::gpar(fill =
        grDevices::adjustcolor("black", alpha.f = 0), col = NA)
    )
    grid::grid.text(
      label = text,
      x = 0.5, y = 0.5,
      default.units = "npc",
      just = c("center", "center"),
      gp = grid::gpar(
        fontsize = size,
        lineheight = lineheight,
        cex = 1,
        col = "black",
        fontfamily = font
      )
    )
    dev.off()
    temp = png::readPNG(temp_filename)
    side_edges = temp[c(1,nrow(temp)),,4]
    vert_edges = temp[,c(1,ncol(temp)),4]
    return(c(any(vert_edges > 0), any(side_edges > 0)))
  }
  #First write with no background
  if(check_text_width || check_text_height) {
    test_edge_vec = test_edges(image_width, image_height)
    if(!check_text_height) {
      test_edge_vec[2] = FALSE
    }
    if(!check_text_width) {
      test_edge_vec[1] = FALSE
    }
    while(any(test_edge_vec)) {
      if(test_edge_vec[1]) {
        image_width = image_width * 2
      }
      if(check_text_height) {
        if(test_edge_vec[2]) {
          image_height = image_height * 2
        }
      }
      test_edge_vec = test_edges(image_width, image_height)
    }

    temp = png::readPNG(temp_filename)
    stopifnot(any(temp[,,4] != 0))

    vert_bbox = range(which(apply(temp[,,4],1,sum) != 0))
    hori_bbox = range(which(apply(temp[,,4],2,sum) != 0))

    vert_blank = (vert_bbox[1] - 1) +  (image_height - vert_bbox[2])
    hori_blank = (hori_bbox[1] - 1) +  (image_width - hori_bbox[2])
    if(check_text_height) {
      image_height = image_height - vert_blank + size
    }
    image_width = image_width - hori_blank + size
  }
  #If no edges, proceed with normal render
  if(length(find.package("ragg",quiet=TRUE)) > 0 && use_ragg) {
    ragg::agg_png(temp_filename,
      width = image_width,
      height = image_height,
      units = "px",
      res = 72,
      background = NA
    )
  } else {
    grDevices::png(temp_filename,
      width = image_width,
      height = image_height,
      pointsize = size,
      family = font,
      res = 72,
      bg = NA
    )
  }
  grid::grid.newpage()
  grid::grid.rect(
    x = 0.5, y = 0.5,
    width = 1,
    height = 1,
    just = c("center"),
    default.units = "npc",
    gp = grid::gpar(fill =
      grDevices::adjustcolor(bg_col, alpha.f = background_alpha), col = NA)
  )
  grid::grid.text(
    label = text,
    x = 0.5, y = 0.5,
    default.units = "npc",
    just = c("center", "center"),
    gp = grid::gpar(
      fontsize = size,
      lineheight = lineheight,
      cex = 1,
      col = convert_color(color, as_hex = TRUE),
      fontfamily = font

    )
  )
  dev.off()

  temp = png::readPNG(temp_filename)


  if (length(dim(temp)) == 3 && dim(temp)[3] == 2) {
    temparray = array(0, dim = c(nrow(temp), ncol(temp), 3))
    temparray[, , 1] = temp[, , 1]
    temparray[, , 2] = temp[, , 1]
    temparray[, , 3] = temp[, , 1]
    temp = temparray
  }
  if (length(dim(temp)) == 2) {
    temparray = array(0, dim = c(nrow(temp), ncol(temp), 3))
    temparray[, , 1] = temp
    temparray[, , 2] = temp
    temparray[, , 3] = temp
    temp = temparray
  }
  handle_image_output(temp, filename = filename, preview = preview)
}
