#' Convert XYZ to xy chromaticity coordinates
#' @param xyz XYZ vector.
#' @return Numeric xy vector.
#' @keywords internal
xyz_to_xy = function(xyz) {
  stopifnot(is.numeric(xyz), length(xyz) == 3L, all(is.finite(xyz)))
  denom = sum(xyz)
  if (!is.finite(denom) || denom <= 0) {
    stop("XYZ values must have a positive finite sum.")
  }
  unname(c(xyz[1] / denom, xyz[2] / denom))
}

#' Convert xy chromaticity coordinates to XYZ
#' @param xy xy chromaticity vector.
#' @return XYZ vector normalized to Y=1.
#' @keywords internal
xy_to_xyz = function(xy) {
  stopifnot(is.numeric(xy), length(xy) == 2L, all(is.finite(xy)))
  if (xy[2] <= 0) {
    stop("xy y-coordinate must be positive.")
  }
  unname(c(xy[1] / xy[2], 1, (1 - xy[1] - xy[2]) / xy[2]))
}

#' Check whether libopenexr supports metadata
#' @return `TRUE` if `libopenexr::write_exr()` accepts metadata.
#' @keywords internal
libopenexr_supports_metadata = function() {
  "metadata" %in% names(formals(libopenexr::write_exr))
}

#' Normalize EXR chromaticities metadata
#' @param chromaticities EXR chromaticities metadata.
#' @return Named chromaticities list.
#' @keywords internal
normalize_exr_chromaticities = function(chromaticities) {
  if (is.null(chromaticities)) {
    return(NULL)
  }

  if (is.list(chromaticities)) {
    required = c("red", "green", "blue", "white")
    if (!all(required %in% names(chromaticities))) {
      stop(
        "EXR chromaticities metadata must include red, green, blue, and white."
      )
    }
    out = chromaticities[required]
  } else if (
    is.matrix(chromaticities) && all(dim(chromaticities) == c(4L, 2L))
  ) {
    out = list(
      red = chromaticities[1L, ],
      green = chromaticities[2L, ],
      blue = chromaticities[3L, ],
      white = chromaticities[4L, ]
    )
  } else if (is.numeric(chromaticities) && length(chromaticities) == 8L) {
    values = matrix(chromaticities, nrow = 4L, byrow = TRUE)
    out = list(
      red = values[1L, ],
      green = values[2L, ],
      blue = values[3L, ],
      white = values[4L, ]
    )
  } else {
    stop("Unsupported EXR chromaticities metadata shape.")
  }

  lapply(out, function(x) {
    stopifnot(is.numeric(x), length(x) == 2L, all(is.finite(x)))
    unname(c(as.numeric(x[1]), as.numeric(x[2])))
  })
}

#' Convert a colorspace descriptor to EXR chromaticities metadata
#' @param colorspace Colorspace descriptor.
#' @return Named EXR chromaticities list.
#' @keywords internal
colorspace_to_exr_chromaticities = function(colorspace) {
  if (!is.list(colorspace) || is.null(colorspace$primaries)) {
    return(NULL)
  }
  primaries = colorspace$primaries
  if (!all(c("r", "g", "b") %in% names(primaries))) {
    return(NULL)
  }
  normalize_exr_chromaticities(list(
    red = primaries$r,
    green = primaries$g,
    blue = primaries$b,
    white = xyz_to_xy(colorspace$white_xyz)
  ))
}

#' Build a colorspace descriptor from EXR metadata
#' @param metadata EXR metadata list.
#' @return Colorspace descriptor, or `NULL` when chromaticities are absent.
#' @keywords internal
colorspace_from_exr_metadata = function(metadata) {
  if (!is.list(metadata) || is.null(metadata$chromaticities)) {
    return(NULL)
  }
  chromaticities = tryCatch(
    normalize_exr_chromaticities(metadata$chromaticities),
    error = function(e) {
      warning(
        "Ignoring invalid EXR chromaticities metadata: ",
        conditionMessage(e),
        call. = FALSE
      )
      NULL
    }
  )
  if (is.null(chromaticities)) {
    return(NULL)
  }
  make_colorspace(
    name = "OpenEXR",
    primaries = list(
      r = chromaticities$red,
      g = chromaticities$green,
      b = chromaticities$blue
    ),
    white_xy = chromaticities$white,
    white_name = "EXR"
  )
}

#' Get the adopted neutral white point from EXR metadata
#' @param metadata EXR metadata list.
#' @return XYZ white point, or `NULL` when absent.
#' @keywords internal
white_current_from_exr_metadata = function(metadata) {
  if (!is.list(metadata) || is.null(metadata$adoptedNeutral)) {
    return(NULL)
  }
  tryCatch(
    xy_to_xyz(metadata$adoptedNeutral),
    error = function(e) {
      warning(
        "Ignoring invalid EXR adoptedNeutral metadata: ",
        conditionMessage(e),
        call. = FALSE
      )
      NULL
    }
  )
}

#' Preserve EXR metadata not promoted to rayimg attributes
#' @param metadata EXR metadata list.
#' @return EXR metadata list, or `NULL` when no metadata remains.
#' @keywords internal
preserved_exr_metadata = function(metadata) {
  if (!is.list(metadata) || length(metadata) == 0L) {
    return(NULL)
  }

  promoted = c("chromaticities", "adoptedNeutral")
  out = metadata
  out[promoted] = NULL
  if (length(out) == 0L) {
    return(NULL)
  }
  out
}

#' Build EXR metadata from a rayimg
#' @param image A `rayimg`.
#' @param metadata Default `NULL`. User-supplied EXR metadata.
#' @return EXR metadata list.
#' @keywords internal
exr_metadata_from_rayimg = function(image, metadata = NULL) {
  out = attr(image, "exr", exact = TRUE)
  if (!is.list(out)) {
    out = list()
  }
  if (!is.null(metadata)) {
    if (!is.list(metadata)) {
      stop("EXR metadata must be a list.", call. = FALSE)
    }
    out = utils::modifyList(out, metadata)
  }

  chromaticities = colorspace_to_exr_chromaticities(
    attr(image, "colorspace", exact = TRUE)
  )
  if (!is.null(chromaticities)) {
    out$chromaticities = chromaticities
  }

  white_current = attr(image, "white_current", exact = TRUE)
  if (!is.null(white_current)) {
    out$adoptedNeutral = xyz_to_xy(white_current)
  }

  if (length(out) == 0L) {
    return(NULL)
  }
  out
}
