#'@title Generate 2D exponential Distribution
#'
#'@description Generates a 2D exponential distribution, with an optional
#'argument to take the exponential to a user-defined power.
#'
#'@param falloff Default `1`. Falloff of the exponential.
#'@param dim Default `c(11, 11)`. The dimensions of the matrix.
#'@param width Default `3` (`-10` to `10`). The range in which to compute the distribution.
#'@param rescale_unity Default `FALSE`. If `TRUE`, this will rescale the max value to one. Useful
#'if wanting to plot the distribution with `plot_image()`.
#'@import stats
#'@export
#'@examples
#'if(run_documentation()){
#'image(generate_2d_exponential(1,31,3), asp=1)
#'}
generate_2d_exponential = function(falloff = 1, dim = c(11,11), width = 3,
                                   rescale_unity = FALSE) {
  if(length(dim) == 1) {
    dim = c(dim, dim)
  }
  xy_ratio = dim[1]/dim[2]
  if(xy_ratio > 1) {
    x = seq(-width*xy_ratio,width*xy_ratio,length.out = dim[1])
    y = seq(-width,width,length.out = dim[2])
  } else {
    x = seq(-width,width,length.out = dim[1])
    y = seq(-width/xy_ratio,width/xy_ratio,length.out = dim[2])
  }
  testmat = matrix(0,dim[1],dim[2])
  for(i in 1:length(x)) {
    for(j in 1:length(y)) {
      testmat[i,j] = (dexp(sqrt(x[i]^2+y[j]^2),rate=1/falloff))
    }
  }
  if(rescale_unity) {
    temp = testmat/sum(testmat)
    return(temp/max(temp))
  }
  testmat/sum(testmat)
}
