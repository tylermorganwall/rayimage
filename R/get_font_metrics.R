#' @title Get Font Metrics
#'
#' @description
#' Calculates font metrics for a specified font, font size, and style. The function renders specific characters to measure ascender and descender heights, which are used for text alignment and positioning in graphical outputs.
#'
#' @param font A character string specifying the font family (e.g., `"Arial"`, `"Times"`, `"Helvetica"`).
#' @param font_size A numeric value specifying the size of the font in points.
#' @param style A character string specifying the font style, such as `"plain"`, `"italic"`, or `"bold"`. Default is `"plain"`.
#'
#' @return A list containing:
#' \describe{
#'   \item{ascender_adjustment}{Numeric value of the height adjustment needed for ascenders.}
#'   \item{descender_adjustment}{Numeric value of the height adjustment needed for descenders.}
#'   \item{neutral_height}{Numeric value of the height of a neutral character.}
#' }
#'
#' @details
#' The function renders specific characters (`"d"` for ascender, `"g"` for descender, `"x"` for neutral) using the specified font parameters. It calculates the bounding box of each character to determine the necessary adjustments for accurate text positioning.
#'
#' @import grDevices grid png
#' @export
#' @examples
#' # Get height of basic sans font
#' get_font_metrics("sans", 60)
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
    grDevices::png(output_file, width = font_size*10, height = font_size*10, bg = "transparent")
    grid::grid.newpage()
    grid::grid.text(char, x = 0.5, y = 0.5, 
                    gp = grid::gpar(fontfamily = font, fontsize = font_size, fontface = style))
    grDevices::dev.off()
    img = png::readPNG(output_file)
    bounding_box = which(apply(img, 1, sum) != 0)
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
    neutral_height = neutral_height,
    total_height = metrics_descender$bottom - metrics_ascender$top
  )
}