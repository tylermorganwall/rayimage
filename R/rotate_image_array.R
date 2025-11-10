#' Rotate an image array around its center
#'
#' @keywords internal
rotate_image_array = function(img, angle) {
  if (length(dim(img)) < 2) {
    stop("rotate_image_array() expects a matrix or array input")
  }
  if (is.null(angle) || is.na(angle) || !is.finite(angle)) {
    return(img)
  }
  angle = angle %% 360
  if (isTRUE(all.equal(angle, 0))) {
    return(img)
  }
  theta = angle * pi / 180
  cos_t = cos(theta)
  sin_t = sin(theta)

  dims = dim(img)
  if (length(dims) == 2) {
    img = array(img, dim = c(dims, 1))
    dims = dim(img)
  }
  height = dims[1]
  width = dims[2]
  channels = dims[3]

  width_rot = abs(width * cos_t) + abs(height * sin_t)
  height_rot = abs(width * sin_t) + abs(height * cos_t)
  new_width = max(1L, as.integer(ceiling(width_rot)))
  new_height = max(1L, as.integer(ceiling(height_rot)))

  cx_old = (width + 1) / 2
  cy_old = (height + 1) / 2
  cx_new = (new_width + 1) / 2
  cy_new = (new_height + 1) / 2

  x_indices = matrix(rep(seq_len(new_width), each = new_height), nrow = new_height)
  y_indices = matrix(rep(seq_len(new_height), times = new_width), nrow = new_height)

  x_rel = x_indices - cx_new
  y_rel = cy_new - y_indices

  x_src = x_rel * cos_t + y_rel * sin_t + cx_old
  y_src_rel = -x_rel * sin_t + y_rel * cos_t
  y_src = cy_old - y_src_rel

  x_vec = as.vector(x_src)
  y_vec = as.vector(y_src)

  rotated = array(0, dim = c(new_height, new_width, channels))
  for (ch in seq_len(channels)) {
    channel = img[,, ch, drop = TRUE]
    rotated_channel = bilinear_sample_matrix(channel, x_vec, y_vec)
    rotated[,, ch] = matrix(rotated_channel, nrow = new_height, ncol = new_width)
  }
  return(rotated)
}

bilinear_sample_matrix = function(mat, x, y, fill = 0) {
  height = nrow(mat)
  width = ncol(mat)
  result = rep(fill, length(x))
  valid = !is.na(x) & !is.na(y) & x >= 1 & x <= width & y >= 1 & y <= height
  if (!any(valid)) {
    return(result)
  }
  xv = x[valid]
  yv = y[valid]
  x0 = floor(xv)
  y0 = floor(yv)
  x1 = pmin(x0 + 1, width)
  y1 = pmin(y0 + 1, height)
  dx = xv - x0
  dy = yv - y0

  idx00 = cbind(as.integer(y0), as.integer(x0))
  idx10 = cbind(as.integer(y0), as.integer(x1))
  idx01 = cbind(as.integer(y1), as.integer(x0))
  idx11 = cbind(as.integer(y1), as.integer(x1))

  v00 = mat[idx00]
  v10 = mat[idx10]
  v01 = mat[idx01]
  v11 = mat[idx11]

  interp = v00 * (1 - dx) * (1 - dy) +
    v10 * dx * (1 - dy) +
    v01 * (1 - dx) * dy +
    v11 * dx * dy
  result[valid] = interp
  return(result)
}
