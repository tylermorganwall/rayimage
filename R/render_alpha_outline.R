#' @title Render Alpha Outline
#'
#' @description Creates a colored RGBA outline or halo from an image alpha
#' channel or a supplied mask. When `image` is supplied, the original image is
#' composited over the outline by default.
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
#' @param gap_fill Default `2`. Maximum alpha gap width, in pixels, to bridge
#' in the expanded outline alpha. This fills border-connected breaks and notches
#' caused by discrete distance quantization. Set to `0` to disable.
#' @param gap_fill_alpha_threshold Default `0.25`. Alpha threshold used to
#' classify border gaps. Gap filling only modifies pixels below this threshold
#' that are connected to the image exterior; enclosed interior holes surrounded
#' by pixels at or above this threshold are left unchanged.
#' @param color Default `"black"`. Outline color.
#' @param alpha Default `1`. Overall outline alpha multiplier.
#' @param pad Default `0`. Padding in pixels before computing the outline. If
#' `NULL`, padding is computed from `expand`, `edge_softness`, and `blur`.
#' @param composite Default `TRUE`. If `TRUE` and `image` is supplied, composite
#' the outline below the original image. Set to `FALSE` to return the outline
#' alone.
#' @param filename Default `NULL`. File to save the image to. If `NULL` and
#' `preview = FALSE`, returns the output image.
#' @param preview Default `FALSE`. If `TRUE`, display the outline image.
#'
#' @return A `rayimg` RGBA output image with a `padding` attribute.
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
  gap_fill = 2,
  gap_fill_alpha_threshold = 0.25,
  color = "black",
  alpha = 1,
  pad = 0,
  composite = TRUE,
  filename = NULL,
  preview = FALSE
) {
  image_supplied = !is.null(image)
  if (!image_supplied && is.null(mask)) {
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
  if (!is.numeric(gap_fill) || length(gap_fill) != 1 || !is.finite(gap_fill)) {
    stop("`gap_fill` must be a finite numeric scalar.")
  }
  if (
    !is.numeric(gap_fill_alpha_threshold) ||
      length(gap_fill_alpha_threshold) != 1 ||
      !is.finite(gap_fill_alpha_threshold)
  ) {
    stop("`gap_fill_alpha_threshold` must be a finite numeric scalar.")
  }
  if (!is.numeric(alpha) || length(alpha) != 1 || !is.finite(alpha)) {
    stop("`alpha` must be a finite numeric scalar.")
  }
  if (!is.logical(composite) || length(composite) != 1 || is.na(composite)) {
    stop("`composite` must be `TRUE` or `FALSE`.")
  }

  expand = max(0, expand)
  edge_softness = max(.Machine$double.eps, edge_softness)
  blur = max(0, blur)
  gap_fill = as.integer(round(max(0, gap_fill)))
  if (gap_fill_alpha_threshold < 0 || gap_fill_alpha_threshold > 1) {
    stop("`gap_fill_alpha_threshold` must be in [0,1].")
  }
  alpha = min(max(alpha, 0), 1)
  if (is.null(pad)) {
    pad = ceiling(expand + edge_softness + 3 * blur)
  }
  pad = normalize_render_padding(pad)

  if (!is.null(mask) && (is.null(dim(mask)) || length(dim(mask)) != 2)) {
    stop("`mask` must be a matrix.")
  }
  if (!image_supplied) {
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
  if (gap_fill > 0 && expand > 0) {
    halo_alpha = fill_alpha_gaps(
      halo_alpha,
      max_gap = gap_fill,
      alpha_threshold = gap_fill_alpha_threshold
    )
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
  if (composite && image_supplied) {
    if (!all(dim(padded_source)[1:2] == dim(halo_image)[1:2])) {
      stop(
        "`image` and `mask` must have matching dimensions when ",
        "`composite = TRUE`."
      )
    }
    halo_image = composite_alpha_outline_image(
      halo_image = halo_image,
      source_image = padded_source
    )
    attr(halo_image, "padding") = pad
  }
  handle_image_output(halo_image, filename = filename, preview = preview)
}

#' @title Composite Alpha Outline Image
#'
#' @description Internal helper that composites the original source image over
#' the generated outline using source-over alpha compositing. The outline is the
#' backdrop and the source image is the foreground. Colors are treated as
#' straight, non-premultiplied alpha, using
#' `Ao = Af + Ab * (1 - Af)` and
#' `Co = (Cf * Af + Cb * Ab * (1 - Af)) / Ao`. Fully transparent output pixels
#' are forced to black to avoid carrying arbitrary color values.
#' @param halo_image RGBA outline image used as the compositing backdrop.
#' @param source_image RGBA source image, padded to the same dimensions as
#' `halo_image`, used as the compositing foreground.
#' @return A `rayimg` RGBA image containing the source composited over the
#' outline.
#'
#' @keywords internal
composite_alpha_outline_image = function(halo_image, source_image) {
  halo_array = unclass(halo_image)
  source_array = unclass(source_image)

  Cf = source_array[,, 1:3]
  Af = source_array[,, 4]
  Cb = halo_array[,, 1:3]
  Ab = halo_array[,, 4]

  Af3 = array(Af, dim = c(dim(Af), 3))
  Ab3 = array(Ab, dim = c(dim(Ab), 3))
  Ao = pmin(pmax(Af + Ab * (1 - Af), 0), 1)
  num = Cf * Af3 + Cb * Ab3 * array(1 - Af, dim = c(dim(Af), 3))

  eps = 1e-8
  Co = num / array(pmax(Ao, eps), dim = c(dim(Ao), 3))
  if (any(Ao <= eps)) {
    transparent = Ao <= eps
    Co[,, 1][transparent] = 0
    Co[,, 2][transparent] = 0
    Co[,, 3][transparent] = 0
  }

  output = array(0, dim = dim(halo_array))
  output[,, 1:3] = Co
  output[,, 4] = Ao
  rayimg(
    output,
    filetype = attr(halo_image, "filetype"),
    source_linear = attr(halo_image, "source_linear"),
    colorspace = attr(halo_image, "colorspace"),
    white_current = attr(halo_image, "white_current"),
    exposure = attr(halo_image, "exposure", exact = TRUE),
    iso = attr(halo_image, "iso", exact = TRUE)
  )
}

#' @title Fill Border Alpha Gaps
#'
#' @description Internal post-process for small distance-transform
#' quantization artifacts on outline edges. The algorithm first calls
#' `find_border_alpha_gaps()` to mark only low-alpha pixels connected to the
#' image exterior. It then scans horizontal, vertical, and diagonal directions.
#' For each gap width up to `max_gap`, `offset_alpha()` samples the alpha values
#' on opposite sides of each candidate gap. A candidate pixel is raised to the
#' weaker of the two neighboring alpha values, and only if that pixel was marked
#' as border-connected. This bridges edge notches while leaving enclosed
#' interior holes unchanged.
#' @param alpha Numeric alpha matrix.
#' @param max_gap Default `1`. Maximum low-alpha run width, in pixels, to bridge.
#' @param alpha_threshold Default `0.25`. Alpha threshold used to decide which
#' pixels are low-alpha border gaps.
#' @return Numeric alpha matrix with eligible border gaps filled.
#'
#' @keywords internal
fill_alpha_gaps = function(alpha, max_gap = 1, alpha_threshold = 0.25) {
  max_gap = as.integer(round(max_gap))
  if (max_gap <= 0) {
    return(alpha)
  }
  alpha = pmin(pmax(alpha, 0), 1)
  border_gap = find_border_alpha_gaps(alpha, alpha_threshold)
  if (!any(border_gap)) {
    return(alpha)
  }
  directions = list(
    c(0L, 1L),
    c(1L, 0L),
    c(1L, 1L),
    c(1L, -1L)
  )
  for (direction in directions) {
    for (gap_width in seq_len(max_gap)) {
      for (left_distance in seq_len(gap_width)) {
        right_distance = gap_width + 1L - left_distance
        left_alpha = offset_alpha(
          alpha,
          row_offset = -left_distance * direction[1],
          col_offset = -left_distance * direction[2]
        )
        right_alpha = offset_alpha(
          alpha,
          row_offset = right_distance * direction[1],
          col_offset = right_distance * direction[2]
        )
        candidate_alpha = pmin(left_alpha, right_alpha)
        alpha[border_gap] = pmax(alpha[border_gap], candidate_alpha[border_gap])
      }
    }
  }
  pmin(pmax(alpha, 0), 1)
}

#' @title Find Border Alpha Gaps
#'
#' @description Internal helper that classifies which low-alpha pixels are part
#' of the exterior border region. Pixels below `alpha_threshold` are considered
#' low alpha. The algorithm starts from low-alpha pixels on the image boundary
#' and performs an 8-connected flood fill through other low-alpha pixels.
#' Low-alpha regions enclosed by pixels at or above `alpha_threshold` are not
#' reached, so they are protected from `fill_alpha_gaps()`.
#' @param alpha Numeric alpha matrix.
#' @param alpha_threshold Default `0.25`. Pixels below this alpha are considered
#' low-alpha gap pixels.
#' @return Logical matrix marking low-alpha pixels connected to the image
#' exterior.
#'
#' @keywords internal
find_border_alpha_gaps = function(alpha, alpha_threshold = 0.25) {
  alpha_threshold = min(max(alpha_threshold, 0), 1)
  low_alpha = alpha < alpha_threshold
  border_gap = matrix(FALSE, nrow = nrow(alpha), ncol = ncol(alpha))
  if (!any(low_alpha)) {
    return(border_gap)
  }

  border = matrix(FALSE, nrow = nrow(alpha), ncol = ncol(alpha))
  border[1, ] = TRUE
  border[nrow(alpha), ] = TRUE
  border[, 1] = TRUE
  border[, ncol(alpha)] = TRUE
  border_indices = which(low_alpha & border)
  if (length(border_indices) == 0) {
    return(border_gap)
  }

  nr = nrow(alpha)
  nc = ncol(alpha)
  queue = integer(length(alpha))
  queue[seq_along(border_indices)] = border_indices
  queue_start = 1L
  queue_end = length(border_indices)
  border_gap[border_indices] = TRUE

  while (queue_start <= queue_end) {
    idx = queue[queue_start]
    queue_start = queue_start + 1L
    row = ((idx - 1L) %% nr) + 1L
    col = ((idx - 1L) %/% nr) + 1L

    row_min = max(1L, row - 1L)
    row_max = min(nr, row + 1L)
    col_min = max(1L, col - 1L)
    col_max = min(nc, col + 1L)
    for (neighbor_row in row_min:row_max) {
      for (neighbor_col in col_min:col_max) {
        if (neighbor_row == row && neighbor_col == col) {
          next
        }
        neighbor_idx = neighbor_row + (neighbor_col - 1L) * nr
        if (low_alpha[neighbor_idx] && !border_gap[neighbor_idx]) {
          queue_end = queue_end + 1L
          queue[queue_end] = neighbor_idx
          border_gap[neighbor_idx] = TRUE
        }
      }
    }
  }
  border_gap
}

#' @title Offset Alpha Matrix
#'
#' @description Internal shift helper used by `fill_alpha_gaps()`. It returns a
#' same-size alpha matrix shifted by integer row and column offsets, filling
#' newly exposed pixels with zero instead of wrapping. This lets the gap-filling
#' scan compare pixels a fixed distance away on opposite sides of a candidate
#' gap.
#' @param alpha Numeric alpha matrix.
#' @param row_offset Integer row offset. Positive values sample from rows below
#' each output pixel.
#' @param col_offset Integer column offset. Positive values sample from columns
#' to the right of each output pixel.
#' @return Numeric alpha matrix with the same dimensions as `alpha`.
#'
#' @keywords internal
offset_alpha = function(alpha, row_offset, col_offset) {
  nr = nrow(alpha)
  nc = ncol(alpha)
  output = matrix(0, nrow = nr, ncol = nc)

  row_start = max(1L, 1L - row_offset)
  row_end = min(nr, nr - row_offset)
  col_start = max(1L, 1L - col_offset)
  col_end = min(nc, nc - col_offset)
  if (row_start > row_end || col_start > col_end) {
    return(output)
  }
  dest_rows = row_start:row_end
  dest_cols = col_start:col_end

  output[dest_rows, dest_cols] = alpha[
    dest_rows + row_offset,
    dest_cols + col_offset,
    drop = FALSE
  ]
  output
}
