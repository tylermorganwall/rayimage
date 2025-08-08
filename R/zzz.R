#' Create a rayimg
#'
#' @param x Default `NULL`. The underlying array/matrix to wrap.
#' @param filetype Default `NULL`. Original source type to record (e.g., "png").
#'
#' @return An object of class `c("rayimg", <original class>)` with `filetype` attribute.
#' @export
new_rayimg = function(x = NULL, filetype = NULL) {
  if (is.null(x)) stop("new_rayimg(): 'x' cannot be NULL.")
  attr(x, "filetype") = filetype
  class(x) = c("rayimg", setdiff(class(x), "rayimg"))
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
`[.rayimg` = function(x = NULL, ..., drop = TRUE) {
  y = NextMethod("[")
  attr(y, "filetype") = attr(x, "filetype")
  if (!is.null(dim(y))) {
    class(y) = c("rayimg", setdiff(class(x), "rayimg"))
  }
  y
}

#' Print method for rayimg
#'
#' @param x Default `NULL`. A `rayimg` object.
#' @param ... Default ``. Passed to downstream print methods.
#' @param decimals Default `2`. Number of decimal places to display.
#'
#' @return Invisibly returns `x`.
#' @export
print.rayimg = function(x = NULL, ..., decimals = 2) {
  ft = attr(x, "filetype")
  if (is.null(ft)) ft = "unknown"
  cat(sprintf("<rayimg> original filetype: %s\n", ft))

  # Strip non-structural attributes so base won't print them
  y = x
  attr(y, "filetype") = NULL
  attrs = attributes(y)
  attributes(y) = attrs[intersect(names(attrs), c("dim", "dimnames"))]

  # Reduce precision
  if (is.numeric(y)) {
    y_fmt = array(
      formatC(as.numeric(y), format = "f", digits = decimals),
      dim = dim(y),
      dimnames = dimnames(y)
    )
    print(y_fmt, quote = FALSE, ...)
  } else {
    print(y, ...)
  }
  invisible(x)
}
