#' @title Add Transparent Padding
#'
#' @description Expands an image or matrix canvas by adding constant-value
#' padding around the original image.
#'
#' @param image 3-layer RGB/4-layer RGBA array, `rayimg` class, matrix, or
#' filename of an image.
#' @param pad Default `10`. Padding in pixels. Use a scalar for equal padding on
#' all sides, or `c(top, right, bottom, left)`.
#' @param color Default `"black"`. Padding color. Can be a scalar, RGB/RGBA
#' numeric vector, or R color string.
#' @param alpha Default `NULL`. Padding alpha for images with an alpha channel.
#' If `NULL`, uses the alpha channel supplied in `color` when present, otherwise
#' defaults to `0` when `color` is omitted and `1` when `color` is supplied.
#' Ignored for RGB and matrix inputs.
#' @param filename Default `NULL`. File to save the image to. If `NULL` and
#' `preview = FALSE`, returns the padded image.
#' @param preview Default `FALSE`. If `TRUE`, display the padded image.
#'
#' @return A padded `rayimg` image or matrix.
#' @export
#' @rdname render_padding
#' @examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#'render_padding(dragon, pad = 20) |>
#'  plot_image()
render_padding = function(
  image,
  pad = 10,
  color = "black",
  alpha = NULL,
  filename = NULL,
  preview = FALSE
) {
  if (is.null(image)) {
    stop("`image` must be provided.")
  }

  color_missing = missing(color)
  color = convert_color(color, single = TRUE)
  color_alpha = if (length(color) >= 4 && !color_missing) {
    color[4]
  } else {
    NULL
  }
  if (is.null(alpha)) {
    alpha = if (!is.null(color_alpha)) {
      color_alpha
    } else if (color_missing) {
      0
    } else {
      1
    }
  }
  if (!is.numeric(alpha) || length(alpha) != 1 || !is.finite(alpha)) {
    stop("`alpha` must be a finite numeric scalar.")
  }
  if (alpha < 0 || alpha > 1) {
    stop("`alpha` must be in [0,1].")
  }
  pad = normalize_render_padding(pad)
  pad_values = unname(pad)

  temp_image = ray_read_image(image, convert_to_array = FALSE)
  image_dim = dim(temp_image)
  channels = if (length(image_dim) == 2) {
    1L
  } else {
    image_dim[3]
  }
  fill = switch(
    as.character(channels),
    `1` = color[1],
    `2` = c(color[1], alpha),
    `3` = color[1:3],
    `4` = c(color[1:3], alpha),
    stop("`image` must have 1, 2, 3, or 4 channels.")
  )

  if (length(image_dim) == 2) {
    output = matrix(
      fill[1],
      nrow = image_dim[1] + pad_values[1] + pad_values[3],
      ncol = image_dim[2] + pad_values[2] + pad_values[4]
    )
    output[
      (pad_values[1] + 1):(pad_values[1] + image_dim[1]),
      (pad_values[4] + 1):(pad_values[4] + image_dim[2])
    ] = unclass(temp_image)
  } else {
    output = array(
      0,
      dim = c(
        image_dim[1] + pad_values[1] + pad_values[3],
        image_dim[2] + pad_values[2] + pad_values[4],
        image_dim[3]
      )
    )
    for (i in seq_len(image_dim[3])) {
      output[,, i] = fill[i]
    }
    output[
      (pad_values[1] + 1):(pad_values[1] + image_dim[1]),
      (pad_values[4] + 1):(pad_values[4] + image_dim[2]),
      seq_len(image_dim[3])
    ] = unclass(temp_image)
  }

  output = rayimg(
    output,
    filetype = attr(temp_image, "filetype"),
    source_linear = attr(temp_image, "source_linear"),
    colorspace = attr(temp_image, "colorspace"),
    white_current = attr(temp_image, "white_current"),
    exposure = attr(temp_image, "exposure", exact = TRUE),
    iso = attr(temp_image, "iso", exact = TRUE)
  )
  attr(output, "padding") = pad
  handle_image_output(output, filename = filename, preview = preview)
}

#' @keywords internal
normalize_render_padding = function(pad) {
  if (!is.numeric(pad) || !(length(pad) %in% c(1, 4))) {
    stop("`pad` must be a numeric scalar or length-4 vector.")
  }
  if (any(!is.finite(pad)) || any(pad < 0)) {
    stop("`pad` must contain non-negative finite values.")
  }
  pad = as.integer(round(pad))
  if (length(pad) == 1) {
    pad = rep(pad, 4)
  }
  names(pad) = c("top", "right", "bottom", "left")
  pad
}
