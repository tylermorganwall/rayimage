#' Vertical ellipsis symbol
#'
#' @param use_cli Default `TRUE`. If `TRUE`, consult `cli::is_utf8_output()` for Unicode support.
#' @return "⋮" if Unicode likely supported, otherwise "...".
#' @keywords internal
rayimg_vertical_ellipsis = function(use_cli = TRUE) {
  have_cli = use_cli && requireNamespace("cli", quietly = TRUE)
  unicode_ok = if (have_cli) isTRUE(cli::is_utf8_output()) else
    isTRUE(l10n_info()[["UTF-8"]])
  if (unicode_ok) "\u22EE" else "..."
}

#' @keywords internal
rayimg_truecolor_supported = function() {
  if (!requireNamespace("cli", quietly = TRUE)) return(FALSE)
  dyn = isTRUE(cli::is_dynamic_tty())
  utf8 = isTRUE(cli::is_utf8_output())
  ncol = cli::num_ansi_colors()
  tc = grepl("truecolor|24bit", Sys.getenv("COLORTERM", ""), ignore.case = TRUE)
  dyn && utf8 && (tc || isTRUE(ncol >= 256))
}

#' Compose RGB over checkerboard using alpha
#' @keywords internal
rayimg_composite_checker = function(
  rgb,
  alpha = NULL,
  light = 0.92,
  dark = 0.75
) {
  d = dim(rgb)
  H = d[1]
  W = d[2]
  if (is.null(alpha)) return(rgb)
  pat = outer(seq_len(H), seq_len(W), function(i, j) (i + j) %% 2L == 0L)
  bg = array(dark, dim = c(H, W, 3))
  bg[pat] = light
  out = array(0, dim = c(H, W, 3))
  for (k in 1:3) {
    out[,, k] = rgb[,, k] * alpha + bg[,, k] * (1 - alpha)
  }
  return(out)
}

# Truecolor detection
rayimg_truecolor_supported = function() {
  if (!requireNamespace("cli", quietly = TRUE)) return(FALSE)
  dyn = isTRUE(cli::is_dynamic_tty())
  utf8 = isTRUE(cli::is_utf8_output())
  # Require an explicit hint for 24-bit (COLORTERM), or a TERM that advertises it (rare)
  tc = grepl("truecolor|24bit", Sys.getenv("COLORTERM", ""), ignore.case = TRUE)
  term = Sys.getenv("TERM", "")
  direct = grepl("direct", term, ignore.case = TRUE) # e.g. xterm-direct/kitty-direct
  dyn && utf8 && (tc || direct)
}

# Map RGB [0, 255] to nearest xterm-256 index
rayimg_rgb_to_xterm256 = function(r, g, b) {
  # 6x6x6 color cube
  lev = c(0, 95, 135, 175, 215, 255)
  idx = function(x) {
    which.min((lev - x)^2) - 1L
  }
  ri = idx(r)
  gi = idx(g)
  bi = idx(b)
  cube_idx = 16L + 36L * ri + 6L * gi + bi
  cube_rgb = c(lev[ri + 1L], lev[gi + 1L], lev[bi + 1L])

  # grayscale ramp [232-255] (approx [8-238] step 10)
  gray_val = round((r + g + b) / 3)
  gray_i = as.integer(pmin(23L, pmax(0L, round((gray_val - 8) / 10))))
  gray_idx = 232L + gray_i
  gray_rgb = rep(8L + 10L * gray_i, 3L)

  # choose closer
  d_cube = sum((c(r, g, b) - cube_rgb)^2)
  d_gray = sum((c(r, g, b) - gray_rgb)^2)
  if (d_gray < d_cube) {
    return(gray_idx)
  } else {
    return(cube_idx)
  }
}

# One ANSI cell with 24-bit or 256-color fallback
# style: "auto" pick bg/fg based on luminance; "block"/"bg" force mode; "bg256"/"block256" force 256-color
rayimg_ansi_cell = function(
  r,
  g,
  b,
  style = c("auto", "block", "bg", "bg256", "block256")
) {
  style = match.arg(style)
  r = as.integer(pmin(pmax(round(r), 0L), 255L))
  g = as.integer(pmin(pmax(round(g), 0L), 255L))
  b = as.integer(pmin(pmax(round(b), 0L), 255L))

  tc_ok = rayimg_truecolor_supported()

  # luminance coefs
  Y = 0.2126 * r + 0.7152 * g + 0.0722 * b
  if (style == "auto") {
    style = if (tc_ok) {
      (if (Y < 80) "block" else "bg")
    } else {
      (if (Y < 80) "block256" else "bg256")
    }
  }

  # 24-bit variants
  if (style == "bg" && tc_ok) {
    return(paste0("\033[48;2;", r, ";", g, ";", b, "m", "  ", "\033[0m"))
  }
  if (style == "block" && tc_ok) {
    return(paste0(
      "\033[38;2;",
      r,
      ";",
      g,
      ";",
      b,
      "m",
      "\u2588\u2588",
      "\033[0m"
    ))
  }

  # 256-color fallback
  idx = rayimg_rgb_to_xterm256(r, g, b)
  if (style %in% c("bg256", "bg")) {
    return(paste0("\033[48;5;", idx, "m", "  ", "\033[0m"))
  }
  if (style %in% c("block256", "block")) {
    return(paste0("\033[38;5;", idx, "m", "\u2588\u2588", "\033[0m"))
  }

  # last resort
  "\u2588\u2588"
}
