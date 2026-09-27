#' Add a Small Corner Tagline
#'
#' Render a single line of text in a corner without resizing the image. The
#' default monospace font and size keep the tagline small across resolutions.
#' Text is alpha-composited using the image's colorspace.
#'
#' @param image RGB/RGBA array, `rayimg`, or image filename.
#' @param text Single-line tagline to add.
#' @param corner Default `"bottomright"`. One of `"bottomright"`,
#'   `"bottomleft"`, `"topright"`, or `"topleft"`.
#' @param size Default `NULL`. Font size in pixels at 72 dpi. `NULL` uses
#'   the image width divided by 64, rounded, with a minimum of 6.
#' @param padding Default `NULL`. Inset from both corner edges in pixels.
#'   `NULL` uses the image width divided by 64, rounded, with a minimum of 4.
#' @param font Default `"mono"`. Font family; the default is fixed-width.
#' @param color Default `"white"`. Text color.
#' @param alpha Default `0.7`. Text opacity, between zero and one.
#' @param filename Default `NULL`. Optional output image filename.
#' @param preview Default `FALSE`. Display the result as well as returning it.
#' @return A `rayimg` array, invisibly when saved or previewed. RGB/RGBA array
#'   inputs retain their channel count. Text that does not fit raises an error;
#'   reduce `size` or `padding`, or shorten `text`.
#' @export
#' @examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#' render_tagline(dragon, "Created with rayrender", preview = TRUE)
render_tagline = function(
  image,
  text,
  corner = "bottomright",
  size = NULL,
  padding = NULL,
  font = "mono",
  color = "white",
  alpha = .7,
  filename = NULL,
  preview = FALSE
) {
  corner = match.arg(
    corner,
    c("bottomright", "bottomleft", "topright", "topleft")
  )
  if (
    !is.character(text) ||
      length(text) != 1L ||
      is.na(text) ||
      !nzchar(trimws(text)) ||
      grepl("[\r\n]", text)
  ) {
    stop("`text` must be one nonempty line.")
  }
  if (
    !is.character(font) || length(font) != 1L || is.na(font) || !nzchar(font)
  ) {
    stop("`font` must be one nonempty font family.")
  }
  if (
    !is.numeric(alpha) ||
      length(alpha) != 1L ||
      !is.finite(alpha) ||
      alpha < 0 ||
      alpha > 1
  ) {
    stop("`alpha` must be a number between zero and one.")
  }
  source = ray_read_image(image, convert_to_array = TRUE)
  height = dim(source)[1]
  width = dim(source)[2]
  if (is.null(size)) {
    size = max(6, round(width / 64))
  }
  if (is.null(padding)) {
    padding = max(4, round(width / 64))
  }
  if (
    !is.numeric(size) || length(size) != 1L || !is.finite(size) || size <= 0
  ) {
    stop("`size` must be a positive number.")
  }
  if (
    !is.numeric(padding) ||
      length(padding) != 1L ||
      !is.finite(padding) ||
      padding < 0 ||
      padding != round(padding)
  ) {
    stop("`padding` must be a nonnegative integer.")
  }
  label = render_text_image(
    text,
    size = size,
    font = font,
    color = color,
    background_alpha = 0,
    trim = TRUE
  )
  if (ncol(label) + 2 * padding > width || nrow(label) + 2 * padding > height) {
    stop("Tagline does not fit. Reduce `size` or `padding`, or shorten `text`.")
  }
  right = corner %in% c("bottomright", "topright")
  bottom = corner %in% c("bottomright", "bottomleft")
  # Sprite anchors use inclusive, one-based pixel coordinates. This leaves
  # exactly `padding` untouched rows/columns outside the glyph bounding box.
  coords = c(
    if (right) width - padding else padding + 1,
    if (bottom) height - padding else padding + 1
  )
  anchor = switch(
    corner,
    bottomright = "se",
    bottomleft = "sw",
    topright = "ne",
    topleft = "nw"
  )
  render_sprite_overlay(
    image,
    label,
    overlay_coords = coords,
    overlay_anchor = anchor,
    alpha = alpha,
    filename = filename,
    preview = preview
  )
}
