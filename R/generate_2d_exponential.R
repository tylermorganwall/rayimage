#'@title Generate 2D exponential Distribution
#'
#'@description Generates a 2D exponential distribution, with an optional
#'argument to take the exponential to a user-defined power.
#'
#'@param falloff Default `1`. Falloff of the exponential.
#'@param dim Default `c(11, 11)`. The dimensions of the matrix.
#'@param width Default `3` (`-10` to `10`). The range in which to compute the distribution.
#'@import stats
#'@export
#'@examples
#'#if(interactive()){
#'image(generate_2d_exponential(1,31,3), asp=1)
#'#end}
generate_2d_exponential = function(falloff = 1, dim = c(11,11), width = 3) {
  if(length(dim) == 1) {
    dim = c(dim, dim)
  }
  mindim = min(dim)
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
  testmat/sum(testmat)
}
