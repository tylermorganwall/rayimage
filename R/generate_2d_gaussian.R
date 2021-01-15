#'@title Generate 2D Gaussian Distribution
#'
#'@description Generates a 2D gaussian distribution, with an optional
#'argument to take the gaussian to a user-defined power.
#'
#'@param sd Default `1`. Standard deviation of the normal distribution
#'@param power Default `1`. Power to take the distribution. Higher values will result in a sharper peak.
#'@param dim Default `c(11, 11)`. The dimensions of the matrix.
#'@param width Default `3` (`-10` to `10`). The range in which to compute the distribution.
#'@import stats
#'@export
#'@examples
#'image(generate_2d_gaussian(1,1,31), asp=1)
generate_2d_gaussian = function(sd = 1, power = 1, dim = c(11,11), width = 3) {
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
  if(sd == 0) {
    sd == 0.00001
  }
  for(i in 1:length(x)) {
    for(j in 1:length(y)) {
      testmat[i,j] = (dnorm(power*x[i],mean=0,sd=sd) * dnorm(power*y[j],mean=0,sd=sd))
    }
  }
  testmat/sum(testmat)
}
