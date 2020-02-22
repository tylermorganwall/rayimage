#'@title Generate 2D exponential Distribution
#'
#'@description Generates a 2D exponential distribution, with an optional
#'argument to take the exponential to a user-defined power.
#'
#'@param falloff Default `1`. Falloff of the exponential.
#'@param dim Default `11`. The dimensions of the resulting square matrix.
#'@param width Default `3` (`-10` to `10`). The range in which to compute the distribution.
#'@import stats
#'@export
#'@examples
#'image(generate_2d_exponential(1,31,3), asp=1)
generate_2d_exponential = function(falloff = 1, dim = 11, width = 3) {
  x = seq(-width,width,length.out = dim)
  y = seq(-width,width,length.out = dim)
  testmat = matrix(0,dim,dim)
  for(i in 1:length(x)) {
    for(j in 1:length(y)) {
      testmat[i,j] = (dexp(sqrt(x[i]^2+y[j]^2),rate=1/falloff))
    }
  }
  testmat/sum(testmat)
}
