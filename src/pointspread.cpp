#define STB_IMAGE_RESIZE_IMPLEMENTATION

#include <RcppArmadillo.h>
#include <RProgress.h>
#include "stb_image_resize.h"
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
arma::mat subsample(arma::mat& circle, int size) {
  int binsize = circle.n_cols/size;
  arma::mat subsampled(size,size,arma::fill::zeros);
  if(size == 1) {
    subsampled(0,0) = 1;
    return(subsampled);
  }
  arma::mat temp;
  for(int i = 0; i < size; i++) {
    for(int j = 0; j < size; j++) {
      if(i != size - 1 && j != size - 1) {
        temp = circle.submat(binsize*i,binsize*j,binsize*(i+1),binsize*(j+1));
        subsampled(i,j) = accu(temp)/(temp.n_elem);
      } else if (i == size - 1 && j != size - 1) {
        temp = circle.submat(binsize*i,binsize*j,circle.n_rows-1,binsize*(j+1));
        subsampled(i,j) = accu(temp)/(temp.n_elem);
      } else if (j == size - 1 && i != size - 1) {
        temp = circle.submat(binsize*i,binsize*j,binsize*(i+1),circle.n_cols-1);
        subsampled(i,j) = accu(temp)/(temp.n_elem);
      } else if (i == size - 1 && j == size - 1) {
        temp = circle.submat(binsize*i,binsize*j,circle.n_rows-1,circle.n_cols-1);
        subsampled(i,j) = accu(temp)/(temp.n_elem);
      }
    }
  }
  return(subsampled);
}

arma::mat subsample_interpolate(arma::mat& circle, float mag) {
  //Create two vectors ranging from 1 to ncol/nrow
  arma::vec X = arma::regspace(1, circle.n_cols);
  arma::vec Y = arma::regspace(1, circle.n_rows);
  //Create two vectors ranging from 1 to the ncol/nrow, with step mag
  //e.g. (1 + 0*mag, 1 + mag, 1+2*mag, ... , 1 + floor(end-1)/mag, so that
  // start + floor(end-1) * mag < ncol/nrow

  //These are our new, interpolated values
  arma::vec XI = arma::regspace(X.min(), mag, X.max());
  arma::vec YI = arma::regspace(Y.min(), mag, Y.max());

  //If X1.n_elem is even, remove one element to make it odd
  if(XI.n_elem % 2 == 0) {
    XI.set_size(XI.n_elem-1);
  }
  //If Y1.n_elem is even, remove one element to make it odd

  if(YI.n_elem % 2 == 0) {
    YI.set_size(YI.n_elem-1);
  }
  arma::mat ZI;
  //Interpolate the `circle` values given on the XY grid on the new grid ZI specified
  //by XI and YI
  arma::interp2(X, Y, circle, XI, YI, ZI);
  return(ZI);
}

// [[Rcpp::export]]
arma::mat rayinterp2(arma::mat& image, arma::vec& XI, arma::vec& YI) {
  arma::vec X = arma::regspace(1, image.n_cols);
  arma::vec Y = arma::regspace(1, image.n_rows);
  arma::mat ZI;
  arma::interp2(X, Y, image, XI, YI, ZI);
  return(ZI);
}


// [[Rcpp::export]]
arma::mat resize_image(arma::mat& image, float mag) {
  arma::vec X = arma::regspace(1, image.n_cols);
  arma::vec Y = arma::regspace(1, image.n_rows);
  arma::vec XI = arma::regspace(X.min(), 1.0/mag, X.max());
  arma::vec YI = arma::regspace(Y.min(), 1.0/mag, Y.max());
  arma::mat ZI;
  arma::interp2(X, Y, image, XI, YI, ZI);
  return(ZI);
}

// [[Rcpp::export]]
arma::mat resize_image_xy(arma::mat& image, arma::vec XI, arma::vec YI) {
  arma::vec X = arma::regspace(1, image.n_cols);
  arma::vec Y = arma::regspace(1, image.n_rows);
  arma::mat ZI;
  arma::interp2(X, Y, image, XI, YI, ZI);
  return(ZI);
}

// [[Rcpp::export]]
NumericMatrix resize_matrix_stb(NumericMatrix image, int width, int height, int method) {
  float* resized_image = new float[width * height];
  float* original_image = new float[image.ncol() * image.nrow()];

  for(unsigned int i = 0; i < image.nrow(); i++ ) {
    for(unsigned int j = 0; j < image.ncol(); j++) {
      original_image[i + image.nrow() * j] = image(i,j);
    }
  }
  stbir_filter interp_type;
  switch(method) {
    case 0:
      interp_type = STBIR_FILTER_DEFAULT;
      break;
    case 1:
      interp_type = STBIR_FILTER_BOX;
      break;
    case 2:
      interp_type = STBIR_FILTER_TRIANGLE;
      break;
    case 3:
      interp_type = STBIR_FILTER_CUBICBSPLINE;
      break;
    case 4:
      interp_type = STBIR_FILTER_CATMULLROM;
      break;
    case 5:
      interp_type = STBIR_FILTER_MITCHELL;
      break;
  default:
    interp_type = STBIR_FILTER_MITCHELL;
  }

  stbir_resize_float_generic(original_image, image.nrow(), image.ncol(), 0,
                             resized_image, width, height, 0,
                             1, 0, 0, STBIR_EDGE_WRAP, interp_type, STBIR_COLORSPACE_LINEAR, NULL);
  NumericMatrix resized_mat(width,height);
  for(int i = 0; i < width; i++ ) {
    for(int j = 0; j < height; j++) {
      resized_mat(i,j) = resized_image[i + width * j];
    }
  }
  delete[] resized_image;
  delete[] original_image;

  return(resized_mat);
}

float evaluate_disk(float x) {
  return((cos(x*1.685979) * -22.356787 + sin(x*1.685979) * 85.912460) * exp(-4.892608 *x) +
    (cos(x*4.998496) * 35.918936 + sin(x*4.998496) * -28.875618) * exp(-4.711870 *x) +
    (cos(x*8.244168) * -13.212253 + sin(x*8.244168) * -1.578428) * exp(-4.052795 *x) +
    (cos(x*11.900859) * 0.507991 + sin(x*11.900859) * 1.816328) * exp(-2.929212* x) +
    (cos(x*16.116382) * 0.138051 + sin(x*16.116382) * -0.010000) * exp(-1.512961 *x));
}

// [[Rcpp::export]]
arma::mat generate_disk(float radius, int dim, bool offsetx, bool offsety) {
  arma::mat testmat(dim,dim);
  arma::vec x = arma::linspace(-1,1,dim) * radius;
  arma::vec y = arma::linspace(-1,1,dim) * radius;
  if(offsetx) {
    x -= radius/dim + radius/dim/8;
  }
  if(offsety) {
    y -= radius/dim + radius/dim/8;
  }
  for(int i = 0; i < testmat.n_rows; i++) {
    for(int j = 0; j < testmat.n_cols; j++) {
      testmat(i,j) = fabs(evaluate_disk(pow(x[i],2.0) + pow(y[j],2.0)));
    }
  }
  return(testmat);
}

// [[Rcpp::export]]
arma::mat gen_ellipse(const double intensity, double width, double height) {
  arma::mat ellipse(width,height);
  for (int i = 0; i < width; ++i) {
    for (int j = 0; j < height; ++j) {
      ellipse(i,j) = pow(((double)i - width/2 + 0.5), 2.0) * pow(height/2,2.0) +
        pow(((double)j - height/2 + 0.5),2.0) * pow(width/2,2.0) > pow(width * height, 2.0)/16 ? intensity : 0.0;
    }
  }
  return(ellipse);
}

// [[Rcpp::export]]
arma::mat subsample_rect(arma::mat& rect, int binsx, int binsy) {
  int binsizex = rect.n_rows/binsx;
  int binsizey = rect.n_cols/binsy;
  arma::mat subsampled(binsx,binsy,arma::fill::zeros);
  arma::mat temp;
  for(int i = 0; i < binsx; i++) {
    for(int j = 0; j < binsy; j++) {
      if(i != binsx - 1 && j != binsy - 1) {
        temp = rect.submat(binsizex*i,binsizey*j,binsizex*(i+1),binsizey*(j+1));
        subsampled(i,j) = accu(temp)/(temp.n_elem);
      } else if (i == binsx - 1 && j != binsy - 1) {
        temp = rect.submat(binsizex*i,binsizey*j,rect.n_rows-1,binsizey*(j+1));
        subsampled(i,j) = accu(temp)/(temp.n_elem);
      } else if (j == binsy - 1 && i != binsx - 1) {
        temp = rect.submat(binsizex*i,binsizey*j,binsizex*(i+1),rect.n_cols-1);
        subsampled(i,j) = accu(temp)/(temp.n_elem);
      } else if (i == binsx - 1 && j == binsy - 1) {
        temp = rect.submat(binsizex*i,binsizey*j,rect.n_rows-1,rect.n_cols-1);
        subsampled(i,j) = accu(temp)/(temp.n_elem);
      }
    }
  }
  return(subsampled);
}

// [[Rcpp::export]]
arma::mat gen_circle_psf(const double radius) {
  int size = ceil(radius * 2);
  if(size < 6) {
    size = 7;
  }
  if((size % 2) == 0) {
    size++;
  }
  arma::mat kernel(size,size);
  if(radius == 0) {
    arma::mat zero(1,1,arma::fill::ones);
    return(zero);
  }
  double mean = (size-1)/2;
  for (int i = 0; i < size; ++i) {
    for (int j = 0; j < size; ++j) {
      kernel(i,j) = pow(((double)i - mean),2.0) + pow(((double)j - mean),2.0) < pow(radius,2.0) ? 1.0 : 0.0;
    }
  }
  return(kernel);
}

// [[Rcpp::export]]
bool is_inside(double sizehex, double positionx, double positiony, double sinval, double cosval) {
  double num1 = fabs(cosval* (positionx - sizehex) - sinval* (positiony - sizehex));
  double num2 = fabs(sinval* (positionx - sizehex) + cosval* (positiony - sizehex));
  double minval = sizehex - num1 < sizehex / 2 ?  sizehex - num1 : sizehex / 2;
  return(num2 < sqrt(3.0) * minval);
}

// [[Rcpp::export]]
arma::mat gen_hex_psf(const double radius, const double rotation) {
  int size = ceil(radius * 2);
  if(size < 6) {
    size = 7;
  }
  if((size % 2) == 0) {
    size++;
  }
  arma::mat kernel(size,size);
  if(radius == 0) {
    arma::mat zero(1,1,arma::fill::zeros);
    return(zero);
  }
  const double sinval = sin(rotation);
  const double cosval = cos(rotation);
  double mean = (size-1)/2;
  for (int i = 0; i < size; ++i) {
    for (int j = 0; j < size; ++j) {
      kernel(i,j) = is_inside(mean, i, j, sinval, cosval) ? 1.0 : 0.0;
    }
  }
  return(kernel);
}

// [[Rcpp::export]]
arma::mat psf(const arma::mat& image, const IntegerMatrix blurmatrix,
              const arma::mat& depthmap, double depth, const arma::mat custombokeh,
              int type, double bokehintensity, double bokehlimit,
              double rotation, bool progbar, int channel) {
  int maxsteps = max(blurmatrix);
  int rows = image.n_rows;
  int cols = image.n_cols;
  arma::mat tempkernel;
  std::vector<arma::mat> kernels;
  arma::mat mask;
  if(type == 0) {
    mask = generate_disk(1.18, 501, false, false);
  } else if (type == 1) {
    mask = gen_hex_psf(500, rotation);
  } else if (type == 2) {
    mask = custombokeh;
  }
  int counter = 0;
  int max_dim = mask.n_cols > mask.n_rows ?  mask.n_cols : mask.n_rows;
  for(int i = 0; i < maxsteps+1; i++) {
    tempkernel = subsample_interpolate(mask, (float)max_dim/(float)(i+1));
    tempkernel = tempkernel/accu(tempkernel);
    kernels.push_back(tempkernel);
    counter++;
  }
  int halfwidth = (kernels[counter-1].n_cols-1)/2;
  arma::mat temp;
  arma::mat temp2;
  arma::mat depthmask;
  arma::mat blurmask;
  arma::mat result(image.n_rows, image.n_cols, arma::fill::zeros);
  arma::mat normalize(result.n_rows, result.n_cols, arma::fill::zeros);
  std::string pbtext;
  if(channel == 1) {
    pbtext =  "Rendering Bokeh 1/3 [:bar] ETA: :eta";
  } else if(channel == 2) {
    pbtext =  "Rendering Bokeh 2/3 [:bar] ETA: :eta";
  } else if(channel == 3) {
    pbtext =  "Rendering Bokeh 3/3 [:bar] ETA: :eta";
  }
  RProgress::RProgress pb(pbtext);
  if(progbar) {
    pb.set_total(rows*cols);
  }

  int begini, beginj, endi, endj, temphalfi, temphalfj;
  int beginslicei, endslicei, beginslicej, endslicej;
  for (int i = halfwidth; i < rows-halfwidth; ++i) {

    Rcpp::checkUserInterrupt();
    for (int j = halfwidth; j < cols-halfwidth; ++j) {
      if(progbar) {
        pb.tick();
      }
      temp = kernels[blurmatrix(i,j)]/pow(depthmap(i,j),2);
      if(temp.n_cols == 1) {
        normalize(i,j) += 1/pow(depthmap(i,j),2);
        result(i,j) += image(i,j)/pow(depthmap(i,j),2);
        continue;
      }
      begini = i - (temp.n_rows-1)/2;
      beginj = j - (temp.n_cols-1)/2;
      endi = i + (temp.n_rows-1)/2;
      endj = j + (temp.n_cols-1)/2;
      if(image(i,j) > bokehlimit) {
        temp = temp * bokehintensity;
      }

      result.submat(begini, beginj, endi, endj) += image(i,j) * temp;
      normalize.submat(begini, beginj, endi, endj) += temp;
    }
  }

  for (int i = 0; i < halfwidth; ++i) {
    Rcpp::checkUserInterrupt();
    for (int j = 0; j < cols; ++j) {
      if(progbar) {
        pb.tick();
      }
      temp = kernels[blurmatrix(i,j)]/pow(depthmap(i,j),2);
      if(temp.n_cols == 1) {
        normalize(i,j) += 1/pow(depthmap(i,j),2);
        result(i,j) += image(i,j)/pow(depthmap(i,j),2);
        continue;
      }
      temphalfi =  (temp.n_rows-1)/2;
      temphalfj =  (temp.n_cols-1)/2;
      beginslicei = 0;
      endslicei = temp.n_rows-1;
      beginslicej = 0;
      endslicej = temp.n_cols-1;
      endi = i + temphalfi;
      endj = j + temphalfj;
      begini =  i - temphalfi;
      beginj =  j - temphalfj;
      if(i  - temphalfi < 0) {
        beginslicei = temphalfi - i;
        begini = 0;
      }
      if(i  + temphalfi > rows - 1) {
        endslicei = endslicei - (i  + temphalfi - rows + 1);
        endi = rows - 1;
      }
      if(j  - temphalfj < 0) {
        beginslicej = temphalfj - j;
        beginj = 0;
      }
      if(j  + temphalfj > cols - 1) {
        endslicej = endslicej - (j  + temphalfj - cols + 1);
        endj = cols - 1;
      }
      if(image(i,j) > bokehlimit) {
        temp = temp * bokehintensity;
      }
      temp2 = temp.submat(beginslicei,beginslicej,endslicei,endslicej);
      result.submat(begini, beginj, endi, endj) += image(i,j) * temp2;
      normalize.submat(begini, beginj, endi, endj) += temp2;
    }
  }
  for (int i = halfwidth; i < rows-halfwidth; ++i) {
    Rcpp::checkUserInterrupt();
    for (int j = 0; j < halfwidth; ++j) {
      if(progbar) {
        pb.tick();
      }
      temp = kernels[blurmatrix(i,j)]/pow(depthmap(i,j),2);
      if(temp.n_cols == 1) {
        normalize(i,j) += 1/pow(depthmap(i,j),2);
        result(i,j) += image(i,j)/pow(depthmap(i,j),2);
        continue;
      }
      temphalfi =  (temp.n_rows-1)/2;
      temphalfj =  (temp.n_cols-1)/2;
      beginslicei = 0;
      endslicei = temp.n_rows-1;
      beginslicej = 0;
      endslicej = temp.n_cols-1;
      endi = i + temphalfi;
      endj = j + temphalfj;
      begini =  i - temphalfi;
      beginj =  j - temphalfj;
      if(i  - temphalfi < 0) {
        beginslicei = temphalfi - i;
        begini = 0;
      }
      if(i  + temphalfi > rows - 1) {
        endslicei = endslicei - (i  + temphalfi - rows + 1);
        endi = rows - 1;
      }
      if(j  - temphalfj < 0) {
        beginslicej = temphalfj - j;
        beginj = 0;
      }
      if(j  + temphalfj > cols - 1) {
        endslicej = endslicej - (j  + temphalfj - cols + 1);
        endj = cols - 1;
      }
      if(image(i,j) > bokehlimit) {
        temp = temp * bokehintensity;
      }
      temp2 = temp.submat(beginslicei,beginslicej,endslicei,endslicej);
      result.submat(begini, beginj, endi, endj) += image(i,j) * temp2;
      normalize.submat(begini, beginj, endi, endj) += temp2;
    }
  }
  for (int i = halfwidth; i < rows-halfwidth; ++i) {
    Rcpp::checkUserInterrupt();
    for (int j = cols-halfwidth; j < cols; ++j) {
      if(progbar) {
        pb.tick();
      }
      temp = kernels[blurmatrix(i,j)]/pow(depthmap(i,j),2);
      if(temp.n_cols == 1) {
        normalize(i,j) += 1/pow(depthmap(i,j),2);
        result(i,j) += image(i,j)/pow(depthmap(i,j),2);
        continue;
      }
      temphalfi =  (temp.n_rows-1)/2;
      temphalfj =  (temp.n_cols-1)/2;
      beginslicei = 0;
      endslicei = temp.n_rows-1;
      beginslicej = 0;
      endslicej = temp.n_cols-1;
      endi = i + temphalfi;
      endj = j + temphalfj;
      begini =  i - temphalfi;
      beginj =  j - temphalfj;
      if(i  - temphalfi < 0) {
        beginslicei = temphalfi - i;
        begini = 0;
      }
      if(i  + temphalfi > rows - 1) {
        endslicei = endslicei - (i  + temphalfi - rows + 1);
        endi = rows - 1;
      }
      if(j  - temphalfj < 0) {
        beginslicej = temphalfj - j;
        beginj = 0;
      }
      if(j  + temphalfj > cols - 1) {
        endslicej = endslicej - (j  + temphalfj - cols + 1);
        endj = cols - 1;
      }
      if(image(i,j) > bokehlimit) {
        temp = temp * bokehintensity;
      }
      temp2 = temp.submat(beginslicei,beginslicej,endslicei,endslicej);
      result.submat(begini, beginj, endi, endj) += image(i,j) * temp2;
      normalize.submat(begini, beginj, endi, endj) += temp2;
    }
  }
  for (int i = rows-halfwidth; i < rows; ++i) {
    Rcpp::checkUserInterrupt();
    for (int j = 0; j < cols; ++j) {
      if(progbar) {
        pb.tick();
      }
      temp = kernels[blurmatrix(i,j)]/pow(depthmap(i,j),2);
      if(temp.n_cols == 1) {
        normalize(i,j) += 1/pow(depthmap(i,j),2);
        result(i,j) += image(i,j)/pow(depthmap(i,j),2);
        continue;
      }
      temphalfi =  (temp.n_rows-1)/2;
      temphalfj =  (temp.n_cols-1)/2;
      beginslicei = 0;
      endslicei = temp.n_rows-1;
      beginslicej = 0;
      endslicej = temp.n_cols-1;
      endi = i + temphalfi;
      endj = j + temphalfj;
      begini =  i - temphalfi;
      beginj =  j - temphalfj;
      if(i  - temphalfi < 0) {
        beginslicei = temphalfi - i;
        begini = 0;
      }
      if(i  + temphalfi > rows - 1) {
        endslicei = endslicei - (i  + temphalfi - rows + 1);
        endi = rows - 1;
      }
      if(j  - temphalfj < 0) {
        beginslicej = temphalfj - j;
        beginj = 0;
      }
      if(j  + temphalfj > cols - 1) {
        endslicej = endslicej - (j  + temphalfj - cols + 1);
        endj = cols - 1;
      }
      if(image(i,j) > bokehlimit) {
        temp = temp * bokehintensity;
      }
      temp2 = temp.submat(beginslicei,beginslicej,endslicei,endslicej);
      result.submat(begini, beginj, endi, endj) += image(i,j) * temp2;
      normalize.submat(begini, beginj, endi, endj) += temp2;
    }
  }
  return(result/normalize);
}

// [[Rcpp::export]]
arma::mat convolution_cpp(const arma::mat& image, const arma::mat kernel,
                      bool progbar, int channel, arma::mat& bloom_matrix) {
  int rows = image.n_rows;
  int cols = image.n_cols;
  arma::mat mask;
  //kernel needs to have odd number of cols
  int halfwidth = (kernel.n_cols-1)/2;
  arma::mat temp;
  arma::mat temp2;
  arma::mat result(image.n_rows, image.n_cols, arma::fill::zeros);
  arma::mat normalize(result.n_rows, result.n_cols, arma::fill::zeros);
  std::string pbtext;
  if(channel == 1) {
    pbtext =  "Rendering Bokeh 1/3 [:bar] ETA: :eta";
  } else if(channel == 2) {
    pbtext =  "Rendering Bokeh 2/3 [:bar] ETA: :eta";
  } else if(channel == 3) {
    pbtext =  "Rendering Bokeh 3/3 [:bar] ETA: :eta";
  }
  RProgress::RProgress pb(pbtext);
  if(progbar) {
    pb.set_total(rows*cols);
  }
  bool norm = false;
  if(arma::accu(kernel) != 0) {
    norm = true;
  }

  int begini, beginj, endi, endj, temphalfi, temphalfj;
  int beginslicei, endslicei, beginslicej, endslicej;
  temphalfi =  (kernel.n_rows-1)/2;
  temphalfj =  (kernel.n_cols-1)/2;

  for (int i = halfwidth; i < rows-halfwidth; ++i) {
    Rcpp::checkUserInterrupt();
    for (int j = halfwidth; j < cols-halfwidth; ++j) {
      if(bloom_matrix(i,j)) {
        if(progbar) {
          pb.tick();
        }
        if(kernel.n_cols == 1) {
          normalize(i,j) += 1;
          result(i,j) += image(i,j);
          continue;
        }
        begini = i - (kernel.n_rows-1)/2;
        beginj = j - (kernel.n_cols-1)/2;
        endi = i + (kernel.n_rows-1)/2;
        endj = j + (kernel.n_cols-1)/2;
        result.submat(begini, beginj, endi, endj) += image(i,j) * kernel;
        normalize.submat(begini, beginj, endi, endj) += kernel;
      } else {
        result(i,j) += image(i,j);
        normalize(i,j) += 1;
      }
    }
  }

  for (int i = 0; i < halfwidth; ++i) {
    Rcpp::checkUserInterrupt();
    for (int j = 0; j < cols; ++j) {
      if(bloom_matrix(i,j)) {
        if(progbar) {
          pb.tick();
        }
        if(kernel.n_cols == 1) {
          normalize(i,j) += 1;
          result(i,j) += image(i,j);
          continue;
        }
        beginslicei = 0;
        endslicei = kernel.n_rows-1;
        beginslicej = 0;
        endslicej = kernel.n_cols-1;
        endi = i + temphalfi;
        endj = j + temphalfj;
        begini =  i - temphalfi;
        beginj =  j - temphalfj;
        if(i  - temphalfi < 0) {
          beginslicei = temphalfi - i;
          begini = 0;
        }
        if(i  + temphalfi > rows - 1) {
          endslicei = endslicei - (i  + temphalfi - rows + 1);
          endi = rows - 1;
        }
        if(j  - temphalfj < 0) {
          beginslicej = temphalfj - j;
          beginj = 0;
        }
        if(j  + temphalfj > cols - 1) {
          endslicej = endslicej - (j  + temphalfj - cols + 1);
          endj = cols - 1;
        }
        temp2 = kernel.submat(beginslicei,beginslicej,endslicei,endslicej);
        result.submat(begini, beginj, endi, endj) += image(i,j) * temp2;
        normalize.submat(begini, beginj, endi, endj) += temp2;
      } else {
        result(i,j) += image(i,j);
        normalize(i,j) += 1;
      }
    }
  }
  for (int i = halfwidth; i < rows-halfwidth; ++i) {
    Rcpp::checkUserInterrupt();
    for (int j = 0; j < halfwidth; ++j) {
      if(bloom_matrix(i,j)) {
        if(progbar) {
          pb.tick();
        }
        if(kernel.n_cols == 1) {
          normalize(i,j) += 1;
          result(i,j) += image(i,j);
          continue;
        }
        beginslicei = 0;
        endslicei = kernel.n_rows-1;
        beginslicej = 0;
        endslicej = kernel.n_cols-1;
        endi = i + temphalfi;
        endj = j + temphalfj;
        begini =  i - temphalfi;
        beginj =  j - temphalfj;
        if(i  - temphalfi < 0) {
          beginslicei = temphalfi - i;
          begini = 0;
        }
        if(i  + temphalfi > rows - 1) {
          endslicei = endslicei - (i  + temphalfi - rows + 1);
          endi = rows - 1;
        }
        if(j  - temphalfj < 0) {
          beginslicej = temphalfj - j;
          beginj = 0;
        }
        if(j  + temphalfj > cols - 1) {
          endslicej = endslicej - (j  + temphalfj - cols + 1);
          endj = cols - 1;
        }
        temp2 = kernel.submat(beginslicei,beginslicej,endslicei,endslicej);
        result.submat(begini, beginj, endi, endj) += image(i,j) * temp2;
        normalize.submat(begini, beginj, endi, endj) += temp2;
      } else {
        result(i,j) += image(i,j);
        normalize(i,j) += 1;
      }
    }
  }
  for (int i = halfwidth; i < rows-halfwidth; ++i) {
    Rcpp::checkUserInterrupt();
    for (int j = cols-halfwidth; j < cols; ++j) {
      if(bloom_matrix(i,j)) {
        if(progbar) {
          pb.tick();
        }
        if(kernel.n_cols == 1) {
          normalize(i,j) += 1;
          result(i,j) += image(i,j);
          continue;
        }
        beginslicei = 0;
        endslicei = kernel.n_rows-1;
        beginslicej = 0;
        endslicej = kernel.n_cols-1;
        endi = i + temphalfi;
        endj = j + temphalfj;
        begini =  i - temphalfi;
        beginj =  j - temphalfj;
        if(i  - temphalfi < 0) {
          beginslicei = temphalfi - i;
          begini = 0;
        }
        if(i  + temphalfi > rows - 1) {
          endslicei = endslicei - (i  + temphalfi - rows + 1);
          endi = rows - 1;
        }
        if(j  - temphalfj < 0) {
          beginslicej = temphalfj - j;
          beginj = 0;
        }
        if(j  + temphalfj > cols - 1) {
          endslicej = endslicej - (j  + temphalfj - cols + 1);
          endj = cols - 1;
        }
        temp2 = kernel.submat(beginslicei,beginslicej,endslicei,endslicej);
        result.submat(begini, beginj, endi, endj) += image(i,j) * temp2;
        normalize.submat(begini, beginj, endi, endj) += temp2;
      } else {
        result(i,j) += image(i,j);
        normalize(i,j) += 1;
      }
    }
  }
  for (int i = rows-halfwidth; i < rows; ++i) {
    Rcpp::checkUserInterrupt();
    for (int j = 0; j < cols; ++j) {
      if(bloom_matrix(i,j)) {
        if(progbar) {
          pb.tick();
        }
        if(kernel.n_cols == 1) {
          normalize(i,j) += 1;
          result(i,j) += image(i,j);
          continue;
        }
        beginslicei = 0;
        endslicei = kernel.n_rows-1;
        beginslicej = 0;
        endslicej = kernel.n_cols-1;
        endi = i + temphalfi;
        endj = j + temphalfj;
        begini =  i - temphalfi;
        beginj =  j - temphalfj;
        if(i  - temphalfi < 0) {
          beginslicei = temphalfi - i;
          begini = 0;
        }
        if(i  + temphalfi > rows - 1) {
          endslicei = endslicei - (i  + temphalfi - rows + 1);
          endi = rows - 1;
        }
        if(j  - temphalfj < 0) {
          beginslicej = temphalfj - j;
          beginj = 0;
        }
        if(j  + temphalfj > cols - 1) {
          endslicej = endslicej - (j  + temphalfj - cols + 1);
          endj = cols - 1;
        }
        temp2 = kernel.submat(beginslicei,beginslicej,endslicei,endslicej);
        result.submat(begini, beginj, endi, endj) += image(i,j) * temp2;
        normalize.submat(begini, beginj, endi, endj) += temp2;
      } else {
        result(i,j) += image(i,j);
        normalize(i,j) += 1;
      }
    }
  }
  if(norm) {
    return(result/normalize);
  } else {
    return(result);
  }
}
