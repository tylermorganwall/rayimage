#' Create a rayimg
#'
#' This class will always be:
#'  * A matrix or a 2/3/4 layer array
#'    * 1 Layer(s): c("Greyscale")
#'    * 2 Layer(s): c("Greyscale", "Alpha")
#'    * 3 Layer(s): c("Red", "Green", "Blue")
#'    * 4 Layer(s): c("Red", "Green", "Blue", "Alpha")
#'  * It tracks whether the image needs to be gamma corrected for display
#'  *
#'
#' @param x Default `NULL`. The underlying array/matrix to wrap.
#' @param filetype Default `NULL`. Original source type to record (e.g., "png").
#' @param gamma_correct Default `FALSE`. Whether to gamma correct the image.
#'
#' @return An object of class `c("rayimg", <original class>)` with `filetype` attribute.
#' @keywords internal
rayimg = function(x = NULL, filetype = NULL, gamma_correct = FALSE) {
  stopifnot(inherits(x, "array"))
  if (is.null(x)) stop("rayimg(): 'x' cannot be NULL.")
  x_new = rayimg_validate_dimensions(x)
  if (is.null(attr(x, "channels"))) {
    attr(x_new, "channels") = rayimg_detect_channels(x)
  } else {
    attr(x_new, "channels") = attr(x, "channels")
  }
  attr(x_new, "filetype") = filetype
  attr(x_new, "gamma_corrected") = gamma_correct
  class(x_new) = c("rayimg", setdiff(class(x), "rayimg"))
  x_new
}

rayimg_channels_from_count = function(n) {
  switch(
    as.character(n),
    "1" = c("Greyscale"),
    "2" = c("Greyscale", "Alpha"),
    "3" = c("Red", "Green", "Blue"),
    "4" = c("Red", "Green", "Blue", "Alpha"),
    stop("This should never be hit.")
  )
}

rayimg_detect_channels = function(x) {
  d = dim(x)
  if (length(d) < 3L) {
    return("Greyscale")
  }
  rayimg_channels_from_count(d[3])
}

#Always return a multidimensional (d[3] = 2,3,4) array, or a matrix.
rayimg_validate_dimensions = function(x) {
  valid = FALSE
  d = dim(x)
  stopifnot(!is.null(d))
  stopifnot(length(d) == 2 || length(d) == 3)
  stopifnot(d[1] > 0 && d[2] > 0)
  if (length(d) == 3) {
    if (d[3] == 1) {
      x = x[,, 1] #Drop extra
    } else {
      stopifnot(d[3] %in% c(2, 3, 4))
    }
  }
  return(x)
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
  ch = rayimg_detect_channels(x)
  caller_env = parent.frame() # capture before NextMethod()
  mc = match.call(expand.dots = FALSE)
  dots_expr = as.list(mc$`...`)
  nd = length(dim(x))

  # pad missing subscripts to nd
  if (length(dots_expr) < nd) {
    dots_expr = c(dots_expr, rep(list(quote(expr = )), nd - length(dots_expr)))
  }

  # evaluate indices in caller env; TRUE means "take all" on that dim
  idx = lapply(dots_expr, function(z) {
    if (identical(z, quote(expr = ))) TRUE else eval(z, envir = caller_env)
  })

  # channel count checks only if there is a 3rd dim
  if (nd >= 3L) {
    if (!isTRUE(idx[[3L]]) && length(idx[[3L]]) > 4L) {
      stop("A rayimg can only have four or fewer channels selected at once.")
    }
    if (!isTRUE(idx[[3L]]) && any(idx[[3L]] > 4L)) {
      stop(
        "rayimg objects only have a maximum of 4 channels (RGBA): you passed ",
        max(idx[[3L]])
      )
    }
  }

  # perform actual subset without dropping to preserve dims 1 & 2
  y = NextMethod("[", drop = FALSE)

  # selectively drop only dims >= 3 that are length 1
  if (drop) {
    d = dim(y)
    dn = dimnames(y)
    drop_idx = which(d == 1L & seq_along(d) >= 3L)
    if (length(drop_idx)) {
      keep = setdiff(seq_along(d), drop_idx)
      y = array(y, dim = d[keep], dimnames = dn[keep])
    }
  }

  # carry attributes
  attr(y, "filetype") = attr(x, "filetype")
  attr(y, "gamma_corrected") = attr(x, "gamma_corrected")

  # channels attribute follows the 3rd subscript if present
  # ch = attr(x, "channels")
  if (!is.null(ch)) {
    if (nd >= 3L) {
      ch_idx = idx[[3L]]
      if (isTRUE(ch_idx)) ch_idx = seq_along(ch)
      attr(y, "channels") = ch[ch_idx]
    } else {
      # 2D input: keep existing single-channel label if any
      attr(y, "channels") = ch[seq_len(min(1L, length(ch)))]
    }
  } else {
    attr(y, "channels") = rayimg_detect_channels(y)
  }

  # keep class if still has dims
  if (!is.null(dim(y))) {
    class(y) = unique(c("rayimg", setdiff(class(y), "rayimg")))
  }

  y
}
