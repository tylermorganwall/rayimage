#'@title Render Convolution
#'
#'@description Takes an image and applys a convolution operation to it, using
#'a user-supplied or built-in kernel. Edges are calculated by limiting the size
#'of the kernel to only that overlapping the actual image (renormalizing the
#'kernel for the edges).
#'
#'@param image Image filename or 3-layer RGB array.
#'@param filename The filename of the image to be saved. If this is not given, the image will be plotted instead.
#'@param kernel Default `gaussian`. By default, an 11x11 Gaussian kernel with a mean
#'of `0` and a standard deviation of `1`, running from `-kernel_extent` to `kernel_extent`.
#'If numeric, this will be the standard deviation of the normal distribution. If a
#'matrix, it will be used directly as the convolution kernel (but resized always to be an odd number
#'of columns and rows).
#'@param kernel_dim Default `11`. The dimension of the `gaussian` kernel. Ignored
#'if user specifies their own kernel.
#'@param kernel_extent Default `3`. Extent over which to calculate the kernel.
#'@param min_value Default `NULL`. If numeric, specifies he minimum value (for any color channel)
#'for a pixel to have the convolution performed.
#'@param preview Default `TRUE`. Whether to plot the convolved image, or just to return the values.
#'@param gamma_correction Default `TRUE`. Controls gamma correction when adding colors. Default exponent of 2.2.
#'@param progress Default `TRUE`. Whether to display a progress bar.
#'@return 3-layer RGB array of the processed image.
#'@export
#'@examples
#'#Perform a convolution with the default gaussian kernel
#'
#'plot_image(dragon)
#'
#'#Perform a convolution with the default gaussian kernel
#'render_convolution(dragon)
#'
#'#Increase the width of the kernel
#'\donttest{
#'render_convolution(dragon, kernel = 2, kernel_dim=21,kernel_extent=6)
#'}
#'
#'#Only perform the convolution on bright pixels (bloom)
#'\donttest{
#'render_convolution(dragon, kernel = 5, kernel_dim=24, kernel_extent=24, min_value=1)
#'}
#'
#'#Use a built-in kernel:
#'\donttest{
#'render_convolution(dragon, kernel = generate_2d_exponential(falloff=2, dim=31, width=21))
#'}
#'
#'#We can also apply this function to matrices:
#'\donttest{
#'volcano %>% image()
#'volcano %>%
#'  render_convolution(kernel=generate_2d_gaussian(sd=1,dim=31)) %>%
#'  image()
#'}
#'
#'#Use a custom kernel (in this case, an X shape):
#'custom = diag(10) + (diag(10)[,10:1])
#'\donttest{
#'plot_image(custom)
#'render_convolution(dragon, kernel = custom)
#'}
render_convolution = function(image, kernel = "gaussian",
                              kernel_dim = 11, kernel_extent = 3,
                              min_value=NULL,
                              filename=NULL, preview=FALSE,
                              gamma_correction = TRUE, progress = FALSE) {
  if(!is.null(filename)) {
    if(tools::file_ext(filename) != "png") {
      filename = paste0(filename,".png")
    }
  }
  imagetype = get_file_type(image)
  if(imagetype == "array") {
    temp_image = aperm(image,c(2,1,3))
  }
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
  if(imagetype == "jpg") {
    temp_image = suppressWarnings(aperm(jpeg::readJPEG(image),c(2,1,3)))
  } else if (imagetype == "png"){
    temp_image = suppressWarnings(aperm(png::readPNG(image),c(2,1,3)))
  } else if (imagetype == "matrix") {
    temp_image = t(image)
  }

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
      bloom_matrix[temp_image[,,1] <= min_value] = FALSE
      bloom_matrix[temp_image[,,2] <= min_value] = FALSE
      bloom_matrix[temp_image[,,3] <= min_value] = FALSE
    }
  } else {
    if(!is.null(min_value)) {
      bloom_matrix[temp_image <= min_value] = FALSE
    }
  }
  if(imagetype != "matrix") {
    for(i in 1:3) {
      temp_image[,,i] = flipud(t(convolution_cpp(t(flipud(temp_image[,,i])), kernel = kernel,
        progbar = progress, channel = i, bloom_matrix = t(flipud(bloom_matrix)))))
    }
  } else {
    for(i in 1:3) {
      temp_image = flipud(t(convolution_cpp(t(flipud(temp_image)), kernel = kernel,
        progbar = progress, channel = i, bloom_matrix = t(flipud(bloom_matrix)))))
    }
  }
  if(gamma_correction) {
    temp_image = temp_image ^ (1/2.2)
  }
  if(is.null(filename)) {
    if(preview) {
      temp_image[temp_image > 1] = 1
      temp_image[temp_image < 0] = 0
      if(imagetype != "matrix") {
        plot_image(aperm(temp_image,c(2,1,3)))
      } else {
        plot_image(t(temp_image))
      }
    } else {
      if(imagetype != "matrix") {
        aperm(temp_image,c(2,1,3))
      } else {
        t(temp_image)
      }
    }
  } else {
    temp_image[temp_image > 1] = 1
    temp_image[temp_image < 0] = 0
    if(imagetype != "matrix") {
      save_png(aperm(temp_image,c(2,1,3)),filename)
    } else {
      save_png(t(temp_image),filename)
    }
  }
}