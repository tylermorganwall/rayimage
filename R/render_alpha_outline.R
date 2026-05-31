#' @title Render Alpha Outline
#'
#' @description Creates a colored RGBA outline or halo from an image alpha
#' channel or a supplied mask.
#'
#' @param image Default `NULL`. Image whose alpha channel will be used as the
#' mask. If `mask` is supplied, `image` is only used for metadata.
#' @param mask Default `NULL`. Optional logical or numeric matrix to use as the
#' outline mask instead of `image` alpha.
#' @param expand Default `0`. Number of pixels to expand the mask.
#' @param edge_softness Default `0.1`. Width of the softened halo edge
#' transition, in pixels.
#' @param blur Default `0`. Gaussian blur standard deviation applied to the
#' final outline alpha.
#' @param color Default `"black"`. Outline color.
#' @param alpha Default `1`. Overall outline alpha multiplier.
#' @param pad Default `0`. Padding in pixels before computing the outline. If
#' `NULL`, padding is computed from `expand`, `edge_softness`, and `blur`.
#' @param filename Default `NULL`. File to save the image to. If `NULL` and
#' `preview = FALSE`, returns the outline image.
#' @param preview Default `FALSE`. If `TRUE`, display the outline image.
#'
#' @return A `rayimg` RGBA outline image with a `padding` attribute.
#' @export
#' @examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#'txt = render_text_image("Outline", background_alpha = 0)
#'render_alpha_outline(txt, expand = 4, blur = 1, color = "white") |>
#'  plot_image()
render_alpha_outline = function(
  image = NULL,
  mask = NULL,
  expand = 0,
  edge_softness = 0.1,
  blur = 0,
  color = "black",
  alpha = 1,
  pad = 0,
  filename = NULL,
  preview = FALSE
) {
  if (is.null(image) && is.null(mask)) {
    stop("Either `image` or `mask` must be provided.")
  }
  if (!is.numeric(expand) || length(expand) != 1 || !is.finite(expand)) {
    stop("`expand` must be a finite numeric scalar.")
  }
  if (
    !is.numeric(edge_softness) ||
      length(edge_softness) != 1 ||
      !is.finite(edge_softness)
  ) {
    stop("`edge_softness` must be a finite numeric scalar.")
  }
  if (!is.numeric(blur) || length(blur) != 1 || !is.finite(blur)) {
    stop("`blur` must be a finite numeric scalar.")
  }
  if (!is.numeric(alpha) || length(alpha) != 1 || !is.finite(alpha)) {
    stop("`alpha` must be a finite numeric scalar.")
  }

  expand = max(0, expand)
  edge_softness = max(.Machine$double.eps, edge_softness)
  blur = max(0, blur)
  alpha = min(max(alpha, 0), 1)
  if (is.null(pad)) {
    pad = ceiling(expand + edge_softness + 3 * blur)
  }
  pad = normalize_render_padding(pad)

  if (!is.null(mask) && (is.null(dim(mask)) || length(dim(mask)) != 2)) {
    stop("`mask` must be a matrix.")
  }
  if (is.null(image)) {
    image = array(0, dim = c(nrow(mask), ncol(mask), 4))
  }
  source_image = ray_read_image(image, convert_to_array = TRUE)
  padded_source = render_padding(
    source_image,
    pad = pad,
    color = "black",
    alpha = 0
  )

  if (is.null(mask)) {
    alpha_mask = unclass(padded_source[,, 4])
  } else {
    alpha_mask = render_padding(mask, pad = pad, color = "black")
    alpha_mask = unclass(alpha_mask)
  }
  alpha_mask[alpha_mask > 0] = 1
  alpha_mask[alpha_mask <= 0] = 0

  if (expand > 0) {
    booldistance = render_boolean_distance(alpha_mask)
    inner = expand - edge_softness
    outer = expand + edge_softness
    halo_alpha = matrix(
      0,
      nrow = nrow(alpha_mask),
      ncol = ncol(alpha_mask)
    )
    halo_alpha[booldistance <= inner] = 1
    band = booldistance > inner & booldistance < outer
    halo_alpha[band] = (outer - booldistance[band]) / (outer - inner)
  } else {
    halo_alpha = alpha_mask
  }

  halo_color = convert_color(color, single = TRUE)
  halo_image = array(0, dim = c(dim(alpha_mask), 4))
  halo_image[,, 1] = halo_color[1]
  halo_image[,, 2] = halo_color[2]
  halo_image[,, 3] = halo_color[3]
  halo_image[,, 4] = halo_alpha * alpha

  halo_image = rayimg(
    halo_image,
    filetype = attr(padded_source, "filetype"),
    source_linear = attr(padded_source, "source_linear"),
    colorspace = attr(padded_source, "colorspace"),
    white_current = attr(padded_source, "white_current"),
    exposure = attr(padded_source, "exposure", exact = TRUE),
    iso = attr(padded_source, "iso", exact = TRUE)
  )

  if (blur > 0) {
    halo_image = render_convolution(
      halo_image,
      kernel = generate_2d_gaussian(
        sd = blur,
        dim = 31,
        width = 30
      ),
      include_alpha = TRUE,
      preview = FALSE
    )
    halo_image[,, 1] = halo_color[1]
    halo_image[,, 2] = halo_color[2]
    halo_image[,, 3] = halo_color[3]
  }
  attr(halo_image, "padding") = pad
  handle_image_output(halo_image, filename = filename, preview = preview)
}
