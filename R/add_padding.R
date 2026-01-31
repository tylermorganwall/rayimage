#'@title add_padding
#'
#'@description Adds padding to the matrix
#'
#'@param input A two-dimensional matrix or a 3D array (including `rayimg`).
#'@return Matrix with edges padded
#'@keywords internal
add_padding = function(input) {
  is_rayimg = inherits(input, "rayimg")
  input_array = if (is_rayimg) {
    unclass(input)
  } else {
    input
  }

  if (length(dim(input_array)) == 2) {
    temp = matrix(0, nrow = nrow(input_array) + 2, ncol = ncol(input_array) + 2)
    temp[2:(nrow(temp) - 1), 2:(ncol(temp) - 1)] = input_array
    temp[2:(nrow(temp) - 1), 1] = input_array[, 1]
    temp[1, 2:(ncol(temp) - 1)] = input_array[1, ]
    temp[2:(nrow(temp) - 1), ncol(temp)] = input_array[, ncol(input_array)]
    temp[nrow(temp), 2:(ncol(temp) - 1)] = input_array[nrow(input_array), ]
    temp[1, 1] = temp[1, 2]
    temp[1, ncol(temp)] = temp[1, ncol(temp) - 1]
    temp[nrow(temp), 1] = temp[nrow(temp) - 1, 2]
    temp[nrow(temp), ncol(temp)] = temp[nrow(temp) - 1, ncol(temp)]
  } else {
    d = dim(input_array)
    temp = array(0, dim = c(d[1] + 2, d[2] + 2, d[3]))
    temp[2:(d[1] + 1), 2:(d[2] + 1), ] = input_array
    temp[2:(d[1] + 1), 1, ] = input_array[, 1, ]
    temp[1, 2:(d[2] + 1), ] = input_array[1, , ]
    temp[2:(d[1] + 1), d[2] + 2, ] = input_array[, d[2], ]
    temp[d[1] + 2, 2:(d[2] + 1), ] = input_array[d[1], , ]
    temp[1, 1, ] = temp[1, 2, ]
    temp[1, d[2] + 2, ] = temp[1, d[2] + 1, ]
    temp[d[1] + 2, 1, ] = temp[d[1] + 1, 2, ]
    temp[d[1] + 2, d[2] + 2, ] = temp[d[1] + 1, d[2] + 2, ]
  }

  if (is_rayimg) {
    temp = rayimg(
      temp,
      filetype = attr(input, "filetype"),
      source_linear = attr(input, "source_linear"),
      colorspace = attr(input, "colorspace"),
      white_current = attr(input, "white_current")
    )
  }
  temp
}

#'@title add_multi_padding
#'
#'@description Adds multiple levels padding to the matrix
#'
#'@param input A two-dimensional matrix or a 3D array (including `rayimg`).
#'@param pad Number of padding entries
#'@return Matrix with edges padded
#'@keywords internal
add_multi_padding = function(input, pad = 1) {
  if (length(dim(input)) == 2) {
    temp = matrix(0, nrow = nrow(input) + 2 * pad, ncol = ncol(input) + 2 * pad)
    temp[(1 + pad):(pad + nrow(input)), (1 + pad):(pad + ncol(input))] = input
    temp[(1 + pad):(pad + nrow(input)), 1:pad] = input[, 1]
    for (i in 1:pad) {
      temp[i, (1 + pad):(pad + ncol(input))] = input[1, ]
    }
    for (i in (pad + ncol(input) + 1):ncol(temp)) {
      temp[(1 + pad):(pad + nrow(input)), i] = input[, ncol(input)]
    }
    for (i in (pad + nrow(input) + 1):nrow(temp)) {
      temp[i, (1 + pad):(pad + ncol(input))] = input[nrow(input), ]
    }
    temp[1:pad, 1:pad] = input[1, 1]
    temp[1:pad, (pad + ncol(input) + 1):ncol(temp)] = input[1, ncol(input)]
    temp[(pad + nrow(input) + 1):nrow(temp), 1:pad] = input[nrow(input), 1]
    temp[
      (pad + nrow(input) + 1):nrow(temp),
      (pad + ncol(input) + 1):ncol(temp)
    ] = input[nrow(input), ncol(input)]
    return(temp)
  } else {
    temp_height = list()
    for (i in seq_len(dim(input)[3])) {
      temp_height[[i]] = add_multi_padding(input[,, i], pad)
    }
    temp_array = array(0, dim = c(dim(temp_height[[i]]), dim(input)[3]))
    for (i in seq_len(dim(input)[3])) {
      temp_array[,, i] = temp_height[[i]]
    }
    return(temp_array)
  }
}

#'@title trim_padding
#'
#'@description Trims padding
#'
#'@param input A two-dimensional matrix.
#'@param pad Number of padding entries
#'@return Matrix with edges trimmed
#'@keywords internal
trim_padding = function(input, pad = 1) {
  if (length(dim(input)) == 2) {
    return(input[(1 + pad):(nrow(input) - pad), (1 + pad):(ncol(input) - pad)])
  } else {
    return(input[
      (1 + pad):(nrow(input) - pad),
      (1 + pad):(ncol(input) - pad),
      ,
      drop = FALSE
    ])
  }
}

#'@title pad to fit
#'
#'@description Pads to a rect
#'
#'@return Hillshade with edges trimmed
#'@keywords internal
pad_to_fit = function(dim, kernel) {
  whichdim = which.min(dim)
  if (dim[1] != dim[2]) {
    if (whichdim == 1) {
      extra_rows = dim[2] - dim[1]
      if (extra_rows %% 2 == 0) {
        pad = matrix(0, nrow = extra_rows / 2, ncol = ncol(kernel))
        kernel = rbind(pad, kernel, pad)
      } else {
        pad1 = matrix(0, nrow = floor(extra_rows / 2) + 1, ncol = ncol(kernel))
        pad2 = matrix(0, nrow = floor(extra_rows / 2), ncol = ncol(kernel))
        kernel = rbind(pad1, kernel, pad2)
      }
    } else {
      extra_cols = dim[1] - dim[2]
      if (extra_cols %% 2 == 0) {
        pad = matrix(0, nrow = nrow(kernel), ncol = extra_cols / 2)
        kernel = cbind(pad, kernel, pad)
      } else {
        pad1 = matrix(0, nrow = nrow(kernel), ncol = floor(extra_cols / 2) + 1)
        pad2 = matrix(0, nrow = nrow(kernel), ncol = floor(extra_cols / 2))
        kernel = cbind(pad1, kernel, pad2)
      }
    }
  }
  return(kernel)
}

#'@title expand to fit
#'
#'@description Pads to a rect
#'
#'@return Hillshade with edges trimmed
#'@keywords internal
expand_to_fit = function(dim, kernel) {
  kdim = dim(kernel)
  if (!all(kdim <= dim)) {
    stop("kernel dimensions must be less than or equal to image")
  }
  if (kdim[1] != dim[1]) {
    extra_rows = dim[1] - kdim[1]
    if (extra_rows %% 2 == 0) {
      pad = matrix(0, nrow = extra_rows / 2, ncol = ncol(kernel))
      kernel = rbind(pad, kernel, pad)
    } else {
      pad1 = matrix(0, nrow = floor(extra_rows / 2) + 1, ncol = ncol(kernel))
      pad2 = matrix(0, nrow = floor(extra_rows / 2), ncol = ncol(kernel))
      kernel = rbind(pad1, kernel, pad2)
    }
  }
  if (kdim[2] != dim[2]) {
    extra_cols = dim[2] - kdim[2]
    if (extra_cols %% 2 == 0) {
      pad = matrix(0, nrow = nrow(kernel), ncol = extra_cols / 2)
      kernel = cbind(pad, kernel, pad)
    } else {
      pad1 = matrix(0, nrow = nrow(kernel), ncol = floor(extra_cols / 2) + 1)
      pad2 = matrix(0, nrow = nrow(kernel), ncol = floor(extra_cols / 2))
      kernel = cbind(pad1, kernel, pad2)
    }
  }
  return(kernel)
}
