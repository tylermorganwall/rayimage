#' Print method for rayimg
#'
#' @param x Default `NULL`. A `rayimg` object.
#' @param preview_n Default `10`. Max rows/cols to display in numeric preview.
#' @param decimals Default `3`. Number of decimal places to display in numeric preview.
#' @param color Default `TRUE`. Colorize headers and R/G/B/A numeric cells when supported.
#' @param ansi_preview Default `TRUE`. If `TRUE`, show a 24-bit color mosaic preview.
#' @param ansi_size Default `20`. Mosaic size (N → N×N).
#' @param ansi_style Default `"bg"`. `"bg"` uses background color blocks, `"block"` uses `█`.
#' @param ansi_replace Default `TRUE`. Show only mosaic; `FALSE` shows mosaic then numeric preview.
#' @param ... Default ``. Ignored.
#'
#' @return Invisibly returns `x`.
#' @export
print.rayimg = function(
  x = NULL,
  preview_n = 10,
  decimals = 3,
  color = TRUE,
  ansi_preview = TRUE,
  ansi_size = 20,
  ansi_style = c("bg", "block"),
  ansi_replace = TRUE,
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

  # ----- channel label resolution (slice-aware + grayscale) -----
  slice_channels = attr(x, "rayimg_channels")
  slice_channel = attr(x, "rayimg_channel")
  is_grey = isTRUE(attr(x, "rayimg_grayscale"))

  # Deal with cases where the rayimg was sliced like img[,,c(1,3)]
  if (!is.null(slice_channel) && length(slice_channel) > 1L) {
    if (is.null(slice_channels)) {
      slice_channels = slice_channel
    }
    slice_channel = NULL
  }

  is_slice = !is.null(slice_channel) || !is.null(slice_channels)

  default_labels = switch(
    as.character(c),
    "1" = c("Grey"),
    "2" = c("Grey", "A"),
    "3" = c("R", "G", "B"),
    "4" = c("R", "G", "B", "A"),
    paste0("C", seq_len(c))
  )

  # Effective labels for display
  if (length(d) >= 3L && is_grey) {
    # Collapse to Grey (+A if present)
    chan_labels = if (c >= 4L) c("Grey", "A") else if (c == 2L)
      c("Grey", "A") else "Grey"
  } else if (
    !is.null(slice_channels) && length(d) >= 3L && length(slice_channels) == c
  ) {
    chan_labels = slice_channels
  } else {
    chan_labels = default_labels
  }

  code_of = function(lbl) {
    substr(lbl, 1, 1)
  }
  type_str = if (length(d) >= 3L) {
    paste0(vapply(chan_labels, code_of, ""), collapse = "")
  } else {
    code_of(if (!is.null(slice_channel)) slice_channel else default_labels[1])
  }

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
    "  gamma_correction: ",
    col_green(as.character(attr(x, "gamma_correct"))),
    "\n",
    sep = ""
  )

  # ANSI color mosaic (only if truecolor supported)
  if (isTRUE(ansi_preview) && isTRUE(truecolor_ok) && !is_slice) {
    ansi_style = match.arg(ansi_style)

    in_obj = x
    if (length(d) == 0L) {
      in_obj = matrix(
        rep(as.numeric(x), ansi_size * ansi_size),
        nrow = ansi_size,
        ncol = ansi_size
      )
    } else if (length(d) == 2L) {
      in_obj = as.array(x)
    }

    small = tryCatch(
      render_resized(
        in_obj,
        dims = c(ansi_size, ansi_size),
        preview = FALSE,
        method = "tri"
      ),
      error = function(e) NULL
    )
    if (!is.null(small)) {
      sd = dim(small)

      # Build RGB preview depending on channel
      chan_hint = attr(x, "rayimg_channel") # "R","G","B","A" if known (for 2D)
      if (is.null(sd)) {
        mat = matrix(as.numeric(small), nrow = ansi_size, ncol = ansi_size)
        if (identical(chan_hint, "R")) {
          rgb = array(0, c(ansi_size, ansi_size, 3))
        } else if (identical(chan_hint, "G")) {
          rgb = array(0, c(ansi_size, ansi_size, 3))
        } else if (identical(chan_hint, "B")) {
          rgb = array(0, c(ansi_size, ansi_size, 3))
        } else {
          rgb = array(rep(mat, each = 3L), c(ansi_size, ansi_size, 3))
        }
        if (identical(chan_hint, "R")) rgb[,, 1] = mat
        if (identical(chan_hint, "G")) rgb[,, 2] = mat
        if (identical(chan_hint, "B")) rgb[,, 3] = mat
        alpha = array(1, c(ansi_size, ansi_size))
      } else if (length(sd) == 2L) {
        mat = small
        if (identical(chan_hint, "R")) {
          rgb = array(0, c(sd[1], sd[2], 3))
          rgb[,, 1] = mat
        } else if (identical(chan_hint, "G")) {
          rgb = array(0, c(sd[1], sd[2], 3))
          rgb[,, 2] = mat
        } else if (identical(chan_hint, "B")) {
          rgb = array(0, c(sd[1], sd[2], 3))
          rgb[,, 3] = mat
        } else {
          rgb = array(rep(mat, each = 3L), c(sd[1], sd[2], 3))
        }
        alpha = array(1, c(sd[1], sd[2]))
      } else {
        ch = sd[3]
        if (ch >= 3L) {
          rgb = small[,, 1:3, drop = FALSE]
        } else {
          g = small[,, 1, drop = TRUE]
          rgb = array(rep(g, each = 3L), dim = c(sd[1], sd[2], 3))
        }
        alpha = if (ch >= 4L) {
          small[,, 4, drop = TRUE]
        } else {
          array(1, c(sd[1], sd[2]))
        }
      }
      if (image_type == "exr") {
        rgb = to_srgb(rgb)
      }

      do_comp = !is.null(dim(alpha)) &&
        any(abs(alpha - 1) > 1e-8, na.rm = TRUE) &&
        !identical(chan_hint, "A")
      if (do_comp) {
        rgb = rayimg_composite_checker(
          pmin(pmax(rgb, 0), 1),
          pmin(pmax(alpha, 0), 1),
          light = 0.92,
          dark = 0.75
        )
      } else {
        rgb = pmin(pmax(rgb, 0), 1)
      }
      comp = rgb * 255
      msg = sprintf(
        "Low-resolution [%dx%d] console preview (pass to `plot_image()` for full-res):",
        dim(comp)[1],
        dim(comp)[2]
      )
      cat(col_silver(msg), "\n", sep = "")
      for (i in seq_len(dim(comp)[1])) {
        row_cells = character(dim(comp)[2])
        for (j in seq_len(dim(comp)[2])) {
          row_cells[j] = rayimg_ansi_cell(
            comp[i, j, 1],
            comp[i, j, 2],
            comp[i, j, 3],
            style = ansi_style
          )
        }
        cat(paste0(row_cells, collapse = ""), "\n", sep = "")
      }
      if (isTRUE(ansi_replace)) {
        return(invisible(x))
      }
    }
  }
  # End ANSI preview

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
  has_alpha = (length(d) >= 3L) && ("A" %in% chan_labels)
  alpha_print_idx = if (has_alpha) match("A", chan_labels) else NA_integer_
  alpha_phys_idx = if (has_alpha) {
    if (is_slice) {
      alpha_print_idx
    } else if (c >= 4L) {
      4L
    } else if (c == 2L) {
      2L
    } else {
      NA_integer_
    }
  } else {
    NA_integer_
  }

  alpha_all_one = FALSE
  if (!is.na(alpha_phys_idx)) {
    aplane = ux[,, alpha_phys_idx, drop = TRUE]
    alpha_all_one = all(abs(aplane - 1) <= 1e-8, na.rm = TRUE)
  }
  suppress_alpha = has_alpha && alpha_all_one

  # Colorizers for numeric cells
  col_fun = function(lbl) {
    if (!can_color) return(identity)
    switch(
      lbl,
      "R" = col_red,
      "G" = col_green,
      "B" = col_blue,
      "A" = col_cyan,
      identity
    )
  }
  hdr_col_fun = function(lbl) {
    if (!can_color) return(function(z) z)
    switch(
      lbl,
      "R" = col_red,
      "G" = col_green,
      "B" = col_blue,
      "A" = col_cyan,
      function(z) z
    )
  }

  fmt = function(v) formatC(as.numeric(v), format = "f", digits = decimals)

  build_preview_pair = function(mat_numeric, f_color) {
    tl_plain = matrix(
      fmt(mat_numeric[seq_len(rmax), seq_len(cmax), drop = TRUE]),
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

  # ---- 2-D (single-channel) path: color with slice channel if known ----
  if (length(d) == 2L || c == 1L) {
    chan_hint = if (!is.null(slice_channel)) {
      slice_channel
    } else {
      default_labels[1]
    }
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
    if (suppress_alpha && i == alpha_print_idx) next
    lbl = chan_labels[i]
    cat(hdr_col_fun(lbl)(style_bold(paste0("[", lbl, "]"))), "\n", sep = "")

    phys_idx = if (is_slice) {
      # sliced arrays keep planes in the selected order
      i
    } else {
      # unsliced: map label to canonical plane
      switch(
        lbl,
        "R" = 1L,
        "G" = 2L,
        "B" = 3L,
        "A" = alpha_phys_idx,
        "Grey" = 1L, # identical grey planes
        1L
      )
    }

    pair = build_preview_pair(
      ux[,, phys_idx, drop = TRUE],
      f_color = col_fun(lbl)
    )
    print_matrix_ansi(pair$plain, pair$disp)

    if (
      i < length(chan_labels) &&
        !(suppress_alpha && (i + 1L) == alpha_print_idx)
    ) {
      cat("\n")
    }
  }
  if (suppress_alpha) {
    cat(col_silver(style_bold("[A] No transparency (layer all 1.0)\n")))
  }

  invisible(x)
}
