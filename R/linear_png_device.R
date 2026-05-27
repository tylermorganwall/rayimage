#' @title Select PNG Device
#'
#' @description Internal helper to pick the best available PNG graphics
#' device. Uses `ragg::agg_png()` by default, then falls back to a
#' Cairo-backed `grDevices::png()` device when `use_ragg = FALSE`, and
#' finally to the base PNG device.
#'
#' @param use_ragg Default `TRUE`. If `TRUE`, return `ragg::agg_png()`.
#' Set to `FALSE` to skip `ragg`.
#'
#' @keywords internal
#' @importFrom ragg agg_png
linear_png_device = function(use_ragg = TRUE) {
  if (isTRUE(use_ragg)) {
    return(function(...) {
      args = list(...)
      args$family = NULL
      do.call(agg_png, args)
    })
  }

  if (isTRUE(capabilities("cairo"))) {
    return(function(...) {
      grDevices::png(..., type = "cairo")
    })
  }

  warning(
    "No cairo device available: install the {ragg} package to ensure correct treatment of gamma when adding text to images.",
    call. = FALSE
  )

  grDevices::png
}
