#'@title Render Convolution FFT
#'
#'@description Takes an image and applys a convolution operation to it, using
#'a user-supplied or built-in kernel. This function uses a fast-fourier transform and
#'does the convolution in the frequency domain, so it should be faster for much larger kernels.
#'
#'@param image Image filename or 3-layer RGB array.
#'@param filename Default `NULL`. The filename of the image to be saved. If this is not given, the image will be plotted instead.
#'@param kernel Default `gaussian`. By default, an 11x11 Gaussian kernel with a mean
#'of `0` and a standard deviation of `1`, running from `-kernel_extent` to `kernel_extent`.
#'If numeric, this will be the standard deviation of the normal distribution. If a
#'matrix, it will be used directly as the convolution kernel (but resized always to be an odd number
#'of columns and rows).
#'@param kernel_dim Default `c(11, 11)`. The dimension of the `gaussian` kernel. Ignored
#'if user specifies their own kernel.
#'@param kernel_extent Default `3`. Extent over which to calculate the kernel.
#'@param absolute Default `TRUE`. Whether to take the absolute value of the convolution.
#'@param pad Default `50`. Amount to pad the image to remove edge effects.
#'@param preview Default `FALSE`. Whether to plot the convolved image, or just to return the values.
#'@param gamma_correction Default `FALSE`. Controls gamma correction when adding colors. Default exponent of 2.2.
#'@return 3-layer RGB array of the processed image.
#'@export
#'@examples
#'if(rayimage:::run_documentation()){
#'#Perform a convolution with the default gaussian kernel
#'plot_image(dragon)
#'}
#'if(rayimage:::run_documentation()){
#'#Perform a convolution with the default gaussian kernel
#'render_convolution_fft(dragon, kernel=0.1,preview = TRUE)
#'}
#'if(rayimage:::run_documentation()){
#'#Increase the width of the kernel
#'render_convolution_fft(dragon, kernel = 2, kernel_dim=21,kernel_extent=6, preview = TRUE)
#'}
#'if(rayimage:::run_documentation()){
#'#Use a built-in kernel:
#'render_convolution_fft(dragon, kernel = generate_2d_exponential(falloff=2, dim=31, width=21),
#'                   preview = TRUE)
#'}
#'if(rayimage:::run_documentation()){
#'#Perform edge detection
#'edge = matrix(c(-1,-1,-1,-1,8,-1,-1,-1,-1),3,3)
#'render_convolution_fft(dragon, kernel = edge, preview = TRUE)
#'}
#'if(rayimage:::run_documentation()){
#'#Perform edge detection with Sobel matrices
#'sobel1 = matrix(c(1,2,1,0,0,0,-1,-2,-1),3,3)
#'sobel2 = matrix(c(1,2,1,0,0,0,-1,-2,-1),3,3,byrow=TRUE)
#'sob1 = render_convolution_fft(dragon, kernel = sobel1)
#'sob2 = render_convolution_fft(dragon, kernel = sobel2)
#'sob_all = sob1 + sob2
#'plot_image(sob_all)
#'}
#'if(rayimage:::run_documentation()){
#'#We can also apply this function to matrices:
#'volcano |> image()
#'volcano |>
#'  render_convolution_fft(kernel=generate_2d_gaussian(sd=1,dim=31)) |>
#'  image()
#'}
#'if(rayimage:::run_documentation()){
#'#Because this function uses the fast-fourier transform, large kernels will be much faster.
#'render_convolution_fft(dragon, kernel = , preview = TRUE)
#'}
#'if(rayimage:::run_documentation()){
#'#Use a custom kernel (in this case, an X shape):
#'custom = diag(10) + (diag(10)[,10:1])
#'#Normalize
#'custom = custom / 20
#'plot_image(custom*20)
#'render_convolution_fft(dragon, kernel = custom, preview = TRUE)
#'}
render_convolution_fft = function(image, kernel = "gaussian",
                                  kernel_dim = c(11, 11),
                                  kernel_extent = 3, absolute = TRUE, pad = 50,
                                  filename=NULL, preview=FALSE,
                                  gamma_correction = FALSE) {
  shift_fft = function(fft_mat) {
    nr = dim(fft_mat)[1]
    nc = dim(fft_mat)[2]
    if(nr %% 2 == 0) {
      nr_mid1 = nr/2
      nr_mid2 = nr/2+1
    } else {
      nr_mid1 = floor(nr/2)
      nr_mid2 = floor(nr/2)+1
    }
    if(nc %% 2 == 0) {
      nc_mid1 = nc/2
      nc_mid2 = nc/2+1
    } else {
      nc_mid1 = floor(nc/2)
      nc_mid2 = floor(nc/2)+1
    }
    fftcorn_nw = fft_mat[1:nr_mid1, 1:nc_mid1]
    fftcorn_ne = fft_mat[1:nr_mid1, nc_mid2:nc]
    fftcorn_sw = fft_mat[nr_mid2:nr, 1:nc_mid1]
    fftcorn_se = fft_mat[nr_mid2:nr,nc_mid2:nc]
    rbind(cbind(fftcorn_se,fftcorn_sw), cbind(fftcorn_ne,fftcorn_nw))
  }
  if(!is.null(filename)) {
    if(tools::file_ext(filename) != "png") {
      filename = paste0(filename,".png")
    }
  }
  imagetype = get_file_type(image)
  temp_image = image

  if(imagetype == "jpg") {
    temp_image = suppressWarnings((jpeg::readJPEG(image)))
  } else if (imagetype == "png"){
    temp_image = suppressWarnings((png::readPNG(image)))
  } else if (imagetype == "matrix") {
    temp_image = t(image)
  }

  if(is.character(kernel)) {
    if(kernel == "gaussian") {
      kernel = generate_2d_gaussian(1,1,kernel_dim,kernel_extent)
    }
  }
  if(is.numeric(kernel) && length(kernel) == 1) {
    kernel = generate_2d_gaussian(kernel,1,kernel_dim,kernel_extent)
  }

  if(any(dim(kernel)[1:2] != dim(temp_image)[1:2])) {
    if(all(dim(kernel)[1:2] <= dim(temp_image)[1:2])) {
      kernel = expand_to_fit(dim(temp_image)[1:2],kernel)
    } else {
      stop("kernel can't have greater dimensions than image")
    }
  }
  if(pad != 0) {
    temp_image = add_multi_padding(temp_image,pad)
    kernel = add_multi_padding(kernel,pad)
  }

  if(any(dim(kernel) > dim(temp_image)[1:2]*2 + 1)) {
    stop("kernel dimensions: ", paste0(dim(kernel),collapse="x"),
         " must not be greater than 2x image dimensions: ", paste0(dim(temp_image)[1:2],collapse="x"),
         ", plus one (here, ", paste0(dim(temp_image)[1:2]*2 + 1, collapse="x"),").")
  }

  if(gamma_correction) {
    temp_image = temp_image^2.2
  }
  temp_fft = temp_image
  if(length(dim(temp_image)) == 2) {
    temp_fft = stats::fft(temp_image)
  } else if (length(dim(temp_image)) == 3) {
    for(i in seq_len(dim(temp_image)[3])) {
      temp_fft[,,i] = stats::fft(temp_image[,,i])
    }
  }

  kernal_fft = stats::fft(kernel)
  vals = Re(temp_fft)
  if(length(dim(temp_fft)) == 3) {
    for(i in 1:(dim(temp_image)[3])) {
      vals[,,i] = shift_fft(Re(stats::fft(temp_fft[,,i] * kernal_fft, inverse = TRUE))/length(vals[,,i]))
    }
  } else {
    vals = shift_fft(Re(stats::fft(temp_fft * kernal_fft, inverse = TRUE))/length(vals))
  }
  if(absolute) {
    vals = abs(vals)
  }

  if(gamma_correction) {
    vals = vals ^ (1/2.2)
  }
  if(pad != 0) {
    vals = trim_padding(vals,pad)
  }
  if(is.null(filename)) {
    if(preview) {
      vals[vals > 1] = 1
      vals[vals < 0] = 0
      plot_image(vals)
    } else {
      if(imagetype == "matrix") {
        t(vals)
      } else {
        vals
      }
    }
  } else {
    vals[vals > 1] = 1
    vals[vals < 0] = 0
    save_png(vals,filename)
  }
}
