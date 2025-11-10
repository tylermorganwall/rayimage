#'@title Render Convolution
#'
#'@description Takes an image and applys a convolution operation to it, using
#'a user-supplied or built-in kernel. Edges are calculated by limiting the size
#'of the kernel to only that overlapping the actual image (renormalizing the
#'kernel for the edges).
#'
#'@param image 3-layer RGB/4-layer RGBA array, `rayimg` class, or filename of an image.
#'@param filename Default `NULL`. The filename of the image to be saved. If this is not given, the image will be plotted instead.
#'@param kernel Default `gaussian`. By default, an 11x11 Gaussian kernel with a mean
#'of `0` and a standard deviation of `1`, running from `-kernel_extent` to `kernel_extent`.
#'If numeric, this will be the standard deviation of the normal distribution. If a
#'matrix, it will be used directly as the convolution kernel (but resized always to be an odd number
#'of columns and rows).
#'@param kernel_dim Default `11`. The dimension of the `gaussian` kernel. Ignored
#'if user specifies their own kernel.
#'@param kernel_extent Default `3`. Extent over which to calculate the kernel.
#'@param absolute Default `TRUE`. Whether to take the absolute value of the convolution.
#'@param min_value Default `NULL`. If numeric, specifies he minimum value (for any color channel)
#'for a pixel to have the convolution performed.
#'@param include_alpha Default `FALSE`. Whether to include the alpha channel in the convolution.
#'@param preview Default `TRUE`. Whether to plot the convolved image, or just to return the values.
#'@param progress Default `TRUE`. Whether to display a progress bar.
#'@return A `rayimg` RGBA array.
#'@export
#'@examples
#'if(run_documentation()){
#'#Perform a convolution with the default gaussian kernel
#'plot_image(dragon)
#'}
#'if(run_documentation()){
#'#Perform a convolution with the default gaussian kernel
#'render_convolution(dragon, preview = TRUE)
#'}
#'if(run_documentation()){
#'#Increase the width of the kernel
#'render_convolution(dragon, kernel = 2, kernel_dim=21,kernel_extent=6, preview = TRUE)
#'}
#'if(run_documentation()){
#'#Perform edge detection using a edge detection kernel
#'edge = matrix(c(-1,-1,-1,-1,8,-1,-1,-1,-1),3,3)
#'render_convolution(render_bw(dragon), kernel = edge, preview = TRUE, absolute=FALSE)
#'}
#'if(run_documentation()){
#'#Perform edge detection with Sobel matrices
#'sobel1 = matrix(c(1,2,1,0,0,0,-1,-2,-1),3,3)
#'sobel2 = matrix(c(1,2,1,0,0,0,-1,-2,-1),3,3,byrow=TRUE)
#'sob1 = render_convolution(render_bw(dragon), kernel = sobel1)
#'sob2 = render_convolution(render_bw(dragon), kernel = sobel2)
#'sob_all = sob1 + sob2
#'plot_image(sob1)
#'plot_image(sob2)
#'plot_image(sob_all)
#'}
#'
#'if(run_documentation()){
#'#Only perform the convolution on bright pixels (bloom)
#'render_convolution(dragon, kernel = 5, kernel_dim=24, kernel_extent=24,
#'                   min_value=1, preview = TRUE)
#'}
#'if(run_documentation()){
#'#Use a built-in kernel:
#'render_convolution(dragon, kernel = generate_2d_exponential(falloff=2, dim=31, width=21),
#'                   preview = TRUE)
#'}
#'if(run_documentation()){
#'#We can also apply this function to matrices:
#'volcano |> image()
#'volcano |>
#'  render_convolution(kernel=generate_2d_gaussian(sd=1,dim=31)) |>
#'  image()
#'}
#'if(run_documentation()){
#'#Use a custom kernel (in this case, an X shape):
#'custom = diag(10) + (diag(10)[,10:1])
#'plot_image(custom)
#'render_convolution(dragon, kernel = custom, preview = TRUE)
#'}
render_convolution = function(
  image,
  kernel = "gaussian",
  kernel_dim = 11,
  kernel_extent = 3,
  absolute = TRUE,
  min_value = NULL,
  include_alpha = FALSE,
  filename = NULL,
  preview = FALSE,
  progress = FALSE
) {
  if (is.character(kernel)) {
    if (kernel == "gaussian") {
      kernel = generate_2d_gaussian(1, 1, kernel_dim, kernel_extent)
    }
  }
  if (is.numeric(kernel) && length(kernel) == 1) {
    kernel = generate_2d_gaussian(kernel, 1, kernel_dim, kernel_extent)
  }
  if (!is.null(dim(kernel)) && all(dim(kernel) >= 2)) {
    if (ncol(kernel) %% 2 == 0) {
      newkernel = matrix(0, ncol = ncol(kernel) + 1, nrow = nrow(kernel))
      newkernel[, 1:ncol(kernel)] = kernel
      kernel = newkernel
    }
    if (nrow(kernel) %% 2 == 0) {
      newkernel = matrix(0, ncol = ncol(kernel), nrow = nrow(kernel) + 1)
      newkernel[1:nrow(kernel), ] = kernel
      kernel = newkernel
    }
  }
  temp_image = ray_read_image(image, convert_to_array = FALSE)
  colorspace = attr(temp_image, "colorspace")
  whitepoint = attr(temp_image, "white_current")
  imagetype = attr(temp_image, "filetype")
  if (any(dim(kernel) > dim(temp_image)[1:2] * 2 + 1)) {
    stop(
      "kernel dimensions: ",
      paste0(dim(kernel), collapse = "x"),
      " must not be greater than 2x image dimensions: ",
      paste0(dim(temp_image)[1:2], collapse = "x"),
      ", plus one (here, ",
      paste0(dim(temp_image)[1:2] * 2 + 1, collapse = "x"),
      ")."
    )
  }

  bloom_matrix = matrix(TRUE, nrow = nrow(temp_image), ncol = ncol(temp_image))
  if (length(dim(temp_image)) == 3) {
    if (!is.null(min_value)) {
      bool_value = rep(TRUE, length = length(temp_image[,, 1]))
      for (i in seq_along(dim(temp_image)[3])) {
        bool_value = bool_value & temp_image[,, i] <= min_value
      }
      bloom_matrix[bool_value] = FALSE
    }
  } else {
    if (!is.null(min_value)) {
      bloom_matrix[temp_image <= min_value] = FALSE
    }
  }
  temp_image_final = temp_image
  if (imagetype != "matrix") {
    channels = dim(temp_image)[3]
    if (!include_alpha && (channels == 2 || channels == 4)) {
      max_channel = channels - 1
    } else {
      max_channel = channels
    }
    for (i in seq_len(max_channel)) {
      temp_image_final[,, i] = convolution_cpp(
        temp_image[,, i],
        kernel = kernel,
        progbar = progress,
        channel = i,
        bloom_matrix = bloom_matrix
      )
    }
  } else {
    temp_image_final = convolution_cpp(
      temp_image,
      kernel = kernel,
      progbar = progress,
      channel = 1,
      bloom_matrix = bloom_matrix
    )
  }
  if (absolute) {
    temp_image_final = abs(temp_image_final)
  }
  final_image = ray_read_image(
    temp_image_final,
    assume_colorspace = colorspace,
    assume_white = whitepoint,
    source_linear = TRUE
  )
  handle_image_output(final_image, filename = filename, preview = preview)
}
