#' @title Get String Dimensions
#'
#' @description
#' Calculates font metrics for a specified font, font size, and style. 
#'
#' @param string The string to be measured.
#' @param font Default `"sans"`. 
#' @param size A numeric value specifying the size of the font in points.
#' @param align Default `"left"``. The string alignment.
#' @param ... Other arguments to pass to `systemfonts::shape_string()``
#'
#' @return A data.frame listing the string dimensions. 
#'
#' @details
#' The function renders specific characters (`"d"` for ascender, `"g"` for descender, `"x"` for neutral) using the specified font parameters. It calculates the bounding box of each character to determine the necessary adjustments for accurate text positioning.
#'
#' @import grDevices grid png
#' @export
#' @examples
#' # Get height of basic sans font
#' get_string_dimensions("This is a string", size=24)
get_string_dimensions = function(string, font = "sans", 
                                 size = 12, align = "center", ...) {
  size_metrics = systemfonts::shape_string(string, family = font, size = size, 
                                           align = align, ...)[["metrics"]]
  return(size_metrics)
}