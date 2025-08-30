#' Create a rayimg
#'
#' @param x Default `NULL`. The underlying array/matrix to wrap.
#' @param filetype Default `NULL`. Original source type to record (e.g., "png").
#' @param gamma_correct Default `FALSE`. Whether to gamma correct the image.
#'
#' @return An object of class `c("rayimg", <original class>)` with `filetype` attribute.
#' @export
new_rayimg = function(x = NULL, filetype = NULL, gamma_correct = FALSE) {
  if (is.null(x)) stop("new_rayimg(): 'x' cannot be NULL.")
  attr(x, "filetype") = filetype
  attr(x, "gamma_corrected") = gamma_correct
  class(x) = c("rayimg", setdiff(class(x), "rayimg"))
  x
}

#' @title (internal) Mark rayimg as grayscale
#' @description Tag an image so the print method treats it as a single Grey channel,
#' even if the underlying data has RGB duplicates and/or alpha.
#' @keywords internal
rayimg_mark_grey = function(x) {
  attr(x, "rayimg_grayscale") = TRUE
  x
}

#' Subset a rayimg without losing attributes
#'
#' @param x Default `NULL`. A `rayimg` object.
#' @param ... Default ``. Standard subsetting indices.
#' @param drop Default `TRUE`. Whether to drop dimensions (as in base `[`).
#'
#' @return A subset of `x`. Preserves the `filetype` attribute. Preserves the
#'   `rayimg` class if the result still has dimensions (array/matrix).
#' @export
`[.rayimg` = function(x, ..., drop = TRUE) {
  call_idx = match.call(expand.dots = FALSE)$...
  k_expr = if (length(call_idx) >= 3) call_idx[[3]] else NULL
  k_expr_exists = TRUE
  tryCatch(
    {
      k_expr
    },
    error = function(e) {
      k_expr_exists <<- FALSE
    }
  )
  if (k_expr_exists) {
    k_expr_exists = !is.null(k_expr_exists)
  }
  y = NextMethod("[")

  # preserve attrs/class
  attr(y, "filetype") = attr(x, "filetype")
  attr(y, "gamma_corrected") = attr(x, "gamma_corrected")

  class(y) = unique(c("rayimg", setdiff(class(y), "rayimg"), "array"))

  # infer original channel names from source
  src_d = dim(x)
  src_c = if (!is.null(src_d) && length(src_d) >= 3L) src_d[3] else 1L
  src_labels = switch(
    as.character(src_c),
    "1" = c("Gray"),
    "2" = c("Gray", "A"),
    "3" = c("R", "G", "B"),
    "4" = c("R", "G", "B", "A"),
    paste0("C", seq_len(src_c))
  )

  # detect 3rd subscript and map to labels
  if (k_expr_exists && src_c >= 1L) {
    idx = tryCatch(eval(k_expr, parent.frame()), error = function(e) NULL)
    if (is.numeric(idx) && all(is.finite(idx))) {
      idx = as.integer(idx)
      idx = idx[idx >= 1L & idx <= src_c]
      if (length(idx) == 1L) {
        attr(y, "rayimg_channel") = src_labels[idx]
        attr(y, "rayimg_channels") = NULL
      } else if (length(idx) > 1L) {
        attr(y, "rayimg_channels") = unname(src_labels[idx])
        attr(y, "rayimg_channel") = NULL
      }
    }
  }
  y
}
