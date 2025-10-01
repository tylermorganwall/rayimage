#' Print method for rayimg
#'
#' @param x Default `NULL`. A `rayimg` object.
#' @param preview_n Default `10`. Max rows/cols to display in numeric preview.
#' @param decimals Default `3`. Number of decimal places to display in numeric preview.
#' @param color Default `TRUE`. Colorize headers and R/G/B/A numeric cells when supported.
#' @param ... Default ``. Ignored.
#'
#' @return Invisibly returns `x`.
#' @export
print.rayimg = function(
  x = NULL,
  preview_n = 10,
  decimals = 3,
  color = TRUE,
  ...
) {
  ft = attr(x, "filetype")
  if (is.null(ft)) ft = "unknown"
  d = dim(x)
  image_type = attr(x, "filetype")

  opt_override = getOption("rayimage.color", NA)
  want_color = isTRUE(color) && !isFALSE(opt_override)
  have_cli = want_color && requireNamespace("cli", quietly = TRUE)
  can_color = have_cli &&
    isTRUE(cli::num_ansi_colors() >= 8) &&
    isTRUE(cli::is_dynamic_tty()) &&
    !isTRUE(getOption("knitr.in.progress")) &&
    !isTRUE(getOption("testthat.in_progress"))
  truecolor_ok = rayimg_truecolor_supported()

  style_bold = function(z) if (can_color) cli::style_bold(z) else z
  col_magenta = function(z) if (can_color) cli::col_magenta(z) else z
  col_yellow = function(z) if (can_color) cli::col_yellow(z) else z
  col_cyan = function(z) if (can_color) cli::col_cyan(z) else z
  col_silver = function(z) if (can_color) cli::col_silver(z) else z
  col_red = function(z) if (can_color) cli::col_red(z) else z
  col_green = function(z) if (can_color) cli::col_green(z) else z
  col_blue = function(z) if (can_color) cli::col_blue(z) else z

  hell = if (
    requireNamespace("cli", quietly = TRUE) &&
      isTRUE(cli::is_utf8_output())
  ) {
    cli::symbol$ellipsis
  } else {
    "..."
  }
  vell = rayimg_vertical_ellipsis(TRUE)

  # Vector fallback
  if (is.null(d)) {
    cat(
      col_magenta(style_bold("<rayimg>")),
      " ",
      col_silver(sprintf("[length %d]", length(x))),
      " ",
      col_yellow("<vector>"),
      "  ",
      "filetype: ",
      col_cyan(ft),
      "\n",
      sep = ""
    )
    y_plain = formatC(as.numeric(x), format = "f", digits = decimals)
    print(y_plain, quote = FALSE, right = TRUE)
    return(invisible(x))
  }

  h = d[1]
  w = d[2]
  c = if (length(d) >= 3) d[3] else 1L

  chan_labels = attr(x, "channels")

  if (!length(chan_labels)) {
    chan_labels = rayimg_detect_channels(x)
  }

  type_str = paste0(chan_labels, collapse = " + ")

  cat(
    col_magenta(style_bold("<rayimg>")),
    " ",
    col_silver(sprintf(
      "[%d x %d%s]",
      h,
      w,
      if (c > 1) paste0(" x ", c) else ""
    )),
    " ",
    col_yellow(paste0("<", type_str, ">")),
    "  ",
    "filetype: ",
    col_cyan(ft),
    "  Linear Data: ",
    col_green(as.character(attr(x, "source_linear"))),
    "\n",
    sep = ""
  )

  # Numeric preview
  rmax = min(h, preview_n)
  cmax = min(w, preview_n)
  more_rows = h > preview_n
  more_cols = w > preview_n
  if (more_rows || more_cols) {
    msg = sprintf(
      "showing [1:%d, 1:%d]%s plus last row/col",
      rmax,
      cmax,
      if (c > 1) ", all channels" else ""
    )
    cat(col_silver(msg), "\n", sep = "")
  }

  ux = unclass(x)

  # Alpha analysis based on displayed labels; get physical alpha plane correctly
  has_alpha = "Alpha" %in% chan_labels
  suppress_alpha = FALSE
  alpha_idx = -1
  if (has_alpha && length(dim(ux)) > 2) {
    alpha_idx = which("Alpha" == chan_labels)
    aplane = ux[,, alpha_idx, drop = FALSE]
    alpha_all_one = all(abs(aplane - 1) <= 1e-8, na.rm = TRUE)
    suppress_alpha = alpha_all_one
  }

  # Colorizers for numeric cells
  col_fun = function(lbl) {
    if (!can_color) return(identity)
    switch(
      lbl,
      "Red" = col_red,
      "Green" = col_green,
      "Blue" = col_blue,
      "Alpha" = col_cyan,
      identity
    )
  }
  hdr_col_fun = function(lbl) {
    if (!can_color) return(function(z) z)
    switch(
      lbl,
      "Red" = col_red,
      "Green" = col_green,
      "Blue" = col_blue,
      "Alpha" = col_cyan,
      function(z) z
    )
  }
  fmt = function(v) formatC(as.numeric(v), format = "f", digits = decimals)

  build_preview_pair = function(mat_numeric, f_color) {
    mat_numeric = unclass(mat_numeric)
    if (is.null(dim(mat_numeric))) {
      print_tl = mat_numeric
    } else {
      print_tl = mat_numeric[seq_len(rmax), seq_len(cmax), drop = TRUE]
    }
    tl_plain = matrix(
      fmt(print_tl),
      nrow = rmax,
      ncol = cmax
    )
    tl_disp = tl_plain

    if (more_cols) {
      last_col_plain = fmt(mat_numeric[seq_len(rmax), w, drop = TRUE])
      ell_col_plain = rep(hell, rmax)
      tl_plain = cbind(tl_plain, ell_col_plain, last_col_plain)
      tl_disp = cbind(
        tl_disp,
        if (can_color) col_silver(ell_col_plain) else ell_col_plain,
        last_col_plain
      )
    }

    if (more_rows) {
      last_row_first_plain = fmt(mat_numeric[h, seq_len(cmax), drop = TRUE])
      if (more_cols) {
        last_row_plain = c(
          last_row_first_plain,
          hell,
          fmt(mat_numeric[h, w, drop = TRUE])
        )
        last_row_disp = c(
          last_row_first_plain,
          if (can_color) col_silver(hell) else hell,
          fmt(mat_numeric[h, w, drop = TRUE])
        )
      } else {
        last_row_plain = last_row_first_plain
        last_row_disp = last_row_first_plain
      }
      ell_row_plain = rep(vell, ncol(tl_plain))
      ell_row_disp = if (can_color) col_silver(ell_row_plain) else ell_row_plain

      tl_plain = rbind(tl_plain, ell_row_plain, last_row_plain)
      tl_disp = rbind(tl_disp, ell_row_disp, last_row_disp)
    }

    cn_base = if (more_cols) {
      c(
        paste0("[,", seq_len(cmax), "]"),
        paste0("[,", hell, "]"),
        paste0("[,", w, "]")
      )
    } else {
      paste0("[,", seq_len(cmax), "]")
    }
    rn_base = if (more_rows) {
      c(
        paste0("[", seq_len(rmax), ",]"),
        paste0("[", vell, ",]"),
        paste0("[", h, ",]")
      )
    } else {
      paste0("[", seq_len(rmax), ",]")
    }

    dimnames(tl_plain) = list(rn_base, cn_base)
    dimnames(tl_disp) = dimnames(tl_plain)

    if (!identical(f_color, identity)) {
      mask_num = !(tl_disp %in% c(hell, vell))
      tl_disp[mask_num] = f_color(tl_disp[mask_num])
    }

    list(plain = tl_plain, disp = tl_disp)
  }

  print_matrix_ansi = function(mat_plain, mat_disp) {
    stopifnot(identical(dim(mat_plain), dim(mat_disp)))
    rn = rownames(mat_plain)
    cn = colnames(mat_plain)
    pad = function(s, w) {
      vis = if (can_color) {
        cli::ansi_nchar(s, type = "width")
      } else {
        nchar(s, type = "width")
      }
      if (vis < w) {
        return(paste0(strrep(" ", w - vis), s))
      } else {
        return(s)
      }
    }
    rn_w = max(nchar(rn, type = "width"))
    col_w = vapply(
      seq_len(ncol(mat_plain)),
      function(j) {
        max(nchar(c(cn[j], mat_plain[, j]), type = "width"))
      },
      integer(1)
    )

    cat(strrep(" ", rn_w + 1))
    for (j in seq_len(ncol(mat_plain))) {
      cat(" ", pad(cn[j], col_w[j]), sep = "")
    }
    cat("\n")
    for (i in seq_len(nrow(mat_plain))) {
      cat(pad(rn[i], rn_w), " ", sep = "")
      for (j in seq_len(ncol(mat_plain))) {
        cat(" ", pad(mat_disp[i, j], col_w[j]), sep = "")
      }
      cat("\n")
    }
  }

  # ---- 2-D (single-channel) path ----
  # This colors slices the appropriate color (e.g. red if it's the red slice)
  if (length(d) == 2L || c == 1L) {
    chan_hint = if (length(chan_labels)) chan_labels[1] else "Grayscale"
    cat(
      hdr_col_fun(chan_hint)(style_bold(paste0("[", chan_hint, "]"))),
      "\n",
      sep = ""
    )
    pair = build_preview_pair(ux, f_color = col_fun(chan_hint))
    print_matrix_ansi(pair$plain, pair$disp)
    return(invisible(x))
  }

  for (i in seq_along(chan_labels)) {
    if (has_alpha && suppress_alpha && i == alpha_idx) next
    lbl = chan_labels[i]
    cat(hdr_col_fun(lbl)(style_bold(paste0("[", lbl, "]"))), "\n", sep = "")
    pair = build_preview_pair(
      ux[,, i, drop = TRUE],
      f_color = col_fun(lbl)
    )
    print_matrix_ansi(pair$plain, pair$disp)

    if (
      i < length(chan_labels) &&
        !(suppress_alpha && (i + 1L) == alpha_idx)
    ) {
      cat("\n")
    }
  }
  if (
    suppress_alpha &&
      chan_labels[length(chan_labels)] == "Alpha"
  ) {
    cat(col_silver(style_bold("[A] No transparency (layer all 1.0)\n")))
  }

  invisible(x)
}
