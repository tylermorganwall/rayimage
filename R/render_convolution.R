#'@title Render Convolution
#'
#'@description Takes an image and applys a convolution operation to it, using
#'a user-supplied or built-in kernel. Edges are calculated by limiting the size
#'of the kernel to only that overlapping the actual image (renormalizing the
#'kernel for the edges).
#'
#'@param image Image filename or 3-layer RGB array.
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
#'@param preview Default `TRUE`. Whether to plot the convolved image, or just to return the values.
#'@param gamma_correction Default `TRUE`. Controls gamma correction when adding colors. Default exponent of 2.2.
#'@param progress Default `TRUE`. Whether to display a progress bar.
#'@return 3-layer RGB array of the processed image.
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
render_convolution = function(image, kernel = "gaussian",
                              kernel_dim = 11, kernel_extent = 3, absolute = TRUE,
                              min_value=NULL,
                              filename=NULL, preview=FALSE,
                              gamma_correction = FALSE, progress = FALSE) {
  if(!is.null(filename)) {
    if(tools::file_ext(filename) != "png") {
      filename = paste0(filename,".png")
    }
  }
  imagetype = get_file_type(image)
  # temp_image = ray_read_image(image)
  if(is.character(kernel)) {
    if(kernel == "gaussian") {
      kernel = generate_2d_gaussian(1,1,kernel_dim,kernel_extent)
    }
  }
  if(is.numeric(kernel) && length(kernel) == 1) {
    kernel = generate_2d_gaussian(kernel,1,kernel_dim,kernel_extent)
  }
  if(!is.null(dim(kernel)) && all(dim(kernel) >= 2)) {
    if(ncol(kernel) %% 2 == 0) {
      newkernel = matrix(0, ncol = ncol(kernel) + 1, nrow = nrow(kernel))
      newkernel[,1:ncol(kernel)] = kernel
      kernel = newkernel
    }
    if(nrow(kernel) %% 2 == 0) {
      newkernel = matrix(0, ncol = ncol(kernel), nrow = nrow(kernel) + 1)
      newkernel[1:nrow(kernel),] = kernel
      kernel = newkernel
    }
  }
  temp_image = ray_read_image(image, convert_to_array = FALSE)

  if(any(dim(kernel) > dim(temp_image)[1:2]*2 + 1)) {
    stop("kernel dimensions: ", paste0(dim(kernel),collapse="x"),
         " must not be greater than 2x image dimensions: ", paste0(dim(temp_image)[1:2],collapse="x"),
         ", plus one (here, ", paste0(dim(temp_image)[1:2]*2 + 1, collapse="x"),").")
  }

  if(gamma_correction) {
    temp_image = temp_image^2.2
  }
  bloom_matrix = matrix(TRUE, nrow = nrow(temp_image), ncol = ncol(temp_image))
  if(imagetype != "matrix") {
    if(!is.null(min_value)) {
      bloom_matrix[temp_image[,,1] <= min_value & temp_image[,,2] <= min_value & temp_image[,,3] <= min_value] = FALSE
    }
  } else {
    if(!is.null(min_value)) {
      bloom_matrix[temp_image <= min_value] = FALSE
    }
  }
  temp_image_final = temp_image
  if(imagetype != "matrix") {
    for(i in seq_len(dim(temp_image)[3])) {
      temp_image_final[,,i] = convolution_cpp(temp_image[,,i], kernel = kernel,
        progbar = progress, channel = i, bloom_matrix = bloom_matrix)
    }
  } else {
    temp_image_final = convolution_cpp(temp_image, kernel = kernel,
      progbar = progress, channel = 1, bloom_matrix = bloom_matrix)
  }
  if(absolute) {
    temp_image_final = abs(temp_image_final)
  }
  if(gamma_correction) {
    temp_image_final ^ (1/2.2)
  }
  handle_image_output(temp_image_final, filename = filename, preview = preview)
}
