#include <Rcpp.h>
using namespace Rcpp;

#define R_RGBA(r,g,b,a) ((r)|((g)<<8)|((b)<<16)|((a)<<24))

// [[Rcpp::export]]
IntegerVector encode_native_image_rcpp_1(NumericMatrix& bw_image) {
  int n = bw_image.nrow() * bw_image.ncol();
  IntegerVector native_vector(n);
  for (int i = 0; i < bw_image.ncol(); i++) {
    for (int j = 0; j < bw_image.nrow(); j++) {
      native_vector[i + j * bw_image.ncol()] = R_RGBA(
        int(bw_image(j,i) * 255),
        int(bw_image(j,i) * 255),
        int(bw_image(j,i) * 255),
        255);
    }
  }
  return native_vector;
}

// [[Rcpp::export]]
IntegerVector encode_native_image_rcpp_3(NumericMatrix& r_image,
                                         NumericMatrix& g_image,
                                         NumericMatrix& b_image) {
  int n = r_image.nrow() * r_image.ncol();
  IntegerVector native_vector(n);
  for (int i = 0; i < r_image.ncol(); i++) {
    for (int j = 0; j < r_image.nrow(); j++) {
      native_vector[i + j * r_image.ncol()] = R_RGBA(
        int(r_image(j,i) * 255),
        int(g_image(j,i) * 255),
        int(b_image(j,i) * 255),
        255);
    }
  }
  return native_vector;
}

// [[Rcpp::export]]
IntegerVector encode_native_image_rcpp_4(NumericMatrix& r_image,
                                         NumericMatrix& g_image,
                                         NumericMatrix& b_image,
                                         NumericMatrix& a_image) {
  int n = r_image.nrow() * r_image.ncol();
  IntegerVector native_vector(n);
  for (int i = 0; i < r_image.ncol(); i++) {
    for (int j = 0; j < r_image.nrow(); j++) {
      native_vector[i + j * r_image.ncol()] = R_RGBA(
        int(r_image(j,i) * 255),
        int(g_image(j,i) * 255),
        int(b_image(j,i) * 255),
        int(a_image(j,i) * 255));
    }
  }
  return native_vector;
}
