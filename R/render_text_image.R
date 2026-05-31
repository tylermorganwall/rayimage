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
#' @param height Default `NA`. User-defined textbox height.
#' @param check_text_width Default `TRUE`. Whether to manually adjust the bounding
#' box of the resulting image to ensure the string bbox is wide enough for the text.
#' Not all systems provide accurate font sizes: this ensures the string is not cut off
#' at the edges, at the cost of needing to repeatedly render the image internally until
#' a suitable image is found.
#' @param check_text_height Default `FALSE`. Whether to manually adjust the bounding
#' box of the resulting image to ensure the string bbox is tall enough for the text.
#' This will ensure a tight vertical bounding box on the text.
#' @param trim Default `FALSE`. If `TRUE`, post-process the rendered image by
#' cropping it to the tight bounding box of the text or emoji pixels. The
#' bounding box is measured from a transparent text mask, so this can also trim
#' images with opaque backgrounds.
#' @param trim_padding Default `0`. Padding in pixels to add after trimming. Use
#' a scalar for equal padding on all sides, or `c(top, right, bottom, left)`.
#' Nonzero padding implies `trim = TRUE`.
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
#'@examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#'   #Generate an image of some text
#'   render_text_image("Some text", preview = TRUE)
#'   #Change the font size
#'   render_text_image("Some text", size = 100, preview = TRUE)
#'   #Change the font color
#'   render_text_image("Some text", size = 100, color="red",preview = TRUE)
#'   #Change the background color and transparency
#'   render_text_image("Some text", size = 50, color="purple",
#'                       background_color="purple", background_alpha = 0.5,
#'                       preview = TRUE)
#'   # Plot an emoji with the agg device.
#'   render_text_image("\U0001F600\U0001F680", size = 50, color = "purple", use_ragg = TRUE,
#'                     background_alpha = 0,
#'                     preview = TRUE)
#'
#'   # Plot an emoji with the agg device and adjust the height and width (which
#'   # is on by default) to be a tight fit.
#'   render_text_image("\U0001F600\U0001F680", size = 50, color = "purple", use_ragg = TRUE,
#'                     background_alpha = 0, check_text_width = TRUE,
#'                     check_text_height = TRUE,
#'                     preview = TRUE)
#'
#'   # Trim the image to the rendered glyphs and then add transparent padding.
#'   render_text_image("\U0001F409", size = 80, use_ragg = TRUE,
#'                     background_alpha = 0, trim = TRUE, trim_padding = 8,
#'                     preview = TRUE)
render_text_image = function(
  text,
  lineheight = 1,
  color = "black",
  size = 12,
  font = "sans",
  just = "left",
  background_color = "white",
  background_alpha = 1,
  use_ragg = TRUE,
  width = NA,
  height = NA,
  filename = NULL,
  check_text_width = TRUE,
  check_text_height = TRUE,
  trim = FALSE,
  trim_padding = 0,
  preview = FALSE
) {
  if (!is.logical(trim) || length(trim) != 1 || is.na(trim)) {
    stop("`trim` must be `TRUE` or `FALSE`.")
  }
  trim_padding = normalize_render_padding(trim_padding)
  trim_padding_values = unname(trim_padding)
  if (any(trim_padding_values > 0)) {
    trim = TRUE
  }
  text_metrics = systemfonts::shape_string(
    text,
    size = size,
    vjust = 0.5,
    res = 72,
    lineheight = lineheight,
    family = font
  )[["metrics"]]
  bg_col = convert_color(background_color, as_hex = TRUE)
  metrics_string = systemfonts::string_metrics_dev(
    text,
    unit = "inches",
    size = size
  ) *
    72
  max_total_width = ceiling(metrics_string$width) + 2
  max_total_height = ceiling(text_metrics$height)
  n_newlines = sum(strsplit(text, "") == "\n")
  if (!is.na(width)) {
    max_total_width = width
  }
  if (!is.na(height)) {
    max_total_height = height
  }
  final_array = array(
    1,
    dim = c(
      max_total_height,
      max_total_width,
      4
    )
  )
  temp_filename = tempfile(fileext = ".png")
  ray_write_image(final_array, temp_filename)

  temp_image = png::readPNG(temp_filename)
  image_width = ncol(temp_image)
  image_height = nrow(temp_image)

  render_text_alpha_mask = function(image_width, image_height) {
    png_device = linear_png_device(use_ragg = use_ragg)
    png_device(
      temp_filename,
      width = image_width,
      height = image_height,
      units = "px",
      pointsize = size,
      family = font,
      res = 72,
      bg = NA
    )
    dev_id = grDevices::dev.cur()
    close_device = function() {
      devs = grDevices::dev.list()
      if (!is.null(devs) && dev_id %in% devs) {
        grDevices::dev.off(which = dev_id)
      }
    }
    on.exit(close_device(), add = TRUE)

    #First write with no background
    grid::grid.newpage()
    grid::grid.rect(
      x = 0.5,
      y = 0.5,
      width = 1,
      height = 1,
      just = c("center"),
      default.units = "npc",
      gp = grid::gpar(
        fill = grDevices::adjustcolor("black", alpha.f = 0),
        col = NA
      )
    )
    grid::grid.text(
      label = text,
      x = 0.5,
      y = 0.5,
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
    close_device()
    temp = png::readPNG(temp_filename)
    temp[,, 4]
  }

  test_edges = function(image_width, image_height) {
    text_alpha = render_text_alpha_mask(image_width, image_height)
    side_edges = text_alpha[c(1, nrow(text_alpha)), ]
    vert_edges = text_alpha[, c(1, ncol(text_alpha))]
    return(c(any(vert_edges > 0), any(side_edges > 0)))
  }

  #First write with no background
  if (check_text_width || check_text_height) {
    test_edge_vec = test_edges(image_width, image_height)
    if (!check_text_height) {
      test_edge_vec[2] = FALSE
    }
    if (!check_text_width) {
      test_edge_vec[1] = FALSE
    }
    while (any(test_edge_vec)) {
      if (test_edge_vec[1]) {
        image_width = image_width * 2
      }
      if (check_text_height) {
        if (test_edge_vec[2]) {
          image_height = image_height * 2
        }
      }
      test_edge_vec = test_edges(image_width, image_height)
    }

    temp = png::readPNG(temp_filename)
    stopifnot(any(temp[,, 4] != 0))

    vert_bbox = range(which(apply(temp[,, 4], 1, sum) != 0))
    hori_bbox = range(which(apply(temp[,, 4], 2, sum) != 0))

    vert_blank = (vert_bbox[1] - 1) + (image_height - vert_bbox[2])
    hori_blank = (hori_bbox[1] - 1) + (image_width - hori_bbox[2])
    if (check_text_height) {
      image_height = image_height - vert_blank + size
    }
    image_width = image_width - hori_blank + size
  }

  #If no edges, proceed with normal render
  png_device = linear_png_device(use_ragg = use_ragg)
  png_device(
    temp_filename,
    width = image_width,
    height = image_height,
    units = "px",
    pointsize = size,
    family = font,
    res = 72,
    bg = NA
  )

  dev_id = grDevices::dev.cur()
  grid::grid.newpage()
  grid::grid.rect(
    x = 0.5,
    y = 0.5,
    width = 1,
    height = 1,
    just = c("center"),
    default.units = "npc",
    gp = grid::gpar(
      fill = grDevices::adjustcolor(bg_col, alpha.f = background_alpha),
      col = NA
    )
  )

  grid::grid.text(
    label = text,
    x = 0.5,
    y = 0.5,
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
  grDevices::dev.off(which = dev_id)

  temp = ray_read_image(temp_filename, reset_camera_settings = TRUE)
  if (trim) {
    text_alpha = render_text_alpha_mask(image_width, image_height)
    text_rows = which(rowSums(text_alpha > 0) > 0)
    text_cols = which(colSums(text_alpha > 0) > 0)
    if (length(text_rows) > 0 && length(text_cols) > 0) {
      temp = temp[
        min(text_rows):max(text_rows),
        min(text_cols):max(text_cols),
        ,
        drop = FALSE
      ]
    }
  }
  if (any(trim_padding_values > 0)) {
    temp = render_padding(
      temp,
      pad = trim_padding,
      color = background_color,
      alpha = background_alpha,
      preview = FALSE
    )
  }

  handle_image_output(temp, filename = filename, preview = preview)
}
