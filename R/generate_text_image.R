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
#'   generate_text_image("Some text", preview = TRUE)
#' }
#' if (run_documentation()) {
#'   #Change the font size
#'   generate_text_image("Some text", size = 100, preview = TRUE)
#' }
#' if (run_documentation()) {
#'   #Change the font color
#'   generate_text_image("Some text", size = 100, color="red",preview = TRUE)
#' }
#' if (run_documentation()) {
#'   #Change the background color and transparency
#'   generate_text_image("Some text", size = 50, color="purple", 
#'                       background_color="purple", background_alpha = 0.5,
#'                       preview = TRUE)
#' }
generate_text_image = function(
    text,
    lineheight = 1,
    color = "black",
    size = 12,
    font = "sans",
    just = "left",
    background_color = "white",
    background_alpha = 1,
    filename = NULL,
    preview = FALSE) {
  text_metrics = systemfonts::shape_string(text,
    size = size, vjust = 0.5,
    res = 72, lineheight = lineheight,
    family = font
  )[["metrics"]]
  bg_col = convert_color(background_color)
  final_array = array(1, dim = c(
    ceiling(text_metrics$height) - 
      floor(text_metrics$top_border) - 
      floor(text_metrics$top_bearing) - 
      floor(text_metrics$bottom_bearing),
    ceiling(text_metrics$width), 4
  ))
  final_array[, , 1] = bg_col[1]
  final_array[, , 2] = bg_col[2]
  final_array[, , 3] = bg_col[3]
  final_array[, , 4] = background_alpha
  temp = tempfile(fileext = ".png")
  ray_write_image(final_array, temp)
  temp_image = png::readPNG(temp)

  grDevices::png(temp,
    width = ncol(temp_image),
    height = nrow(temp_image),
    pointsize = size,
    family = font,
    res = 72
  )

  gp_text = grid::gpar(col = color, fontsize = size)
  grid::grid.newpage()
  plot_image(temp_image, gp = gp_text)
  grid::seekViewport("image")

  grid::grid.text(
    label = text,
    x = 0, y = 0.5,
    default.units = "npc",
    just = c(just, "center"),
    gp = grid::gpar(
      fontsize = size,
      lineheight = lineheight,
      cex = 1,
      col = color,
      fontfamily = font
    )
  )
  dev.off()

  temp = png::readPNG(temp)
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
  if (is.null(filename)) {
    if (!preview) {
      return(temp)
    }
    plot_image(temp)
    return(invisible(temp))
  } else {
    ray_write_image(temp, filename)
    return(invisible(temp))
  }
}
