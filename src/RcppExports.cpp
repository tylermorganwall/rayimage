// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// get_boolean_distance
NumericMatrix get_boolean_distance(LogicalMatrix input);
RcppExport SEXP _rayimage_get_boolean_distance(SEXP inputSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< LogicalMatrix >::type input(inputSEXP);
    rcpp_result_gen = Rcpp::wrap(get_boolean_distance(input));
    return rcpp_result_gen;
END_RCPP
}
// encode_native_image_rcpp_1
IntegerVector encode_native_image_rcpp_1(NumericMatrix& bw_image);
RcppExport SEXP _rayimage_encode_native_image_rcpp_1(SEXP bw_imageSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix& >::type bw_image(bw_imageSEXP);
    rcpp_result_gen = Rcpp::wrap(encode_native_image_rcpp_1(bw_image));
    return rcpp_result_gen;
END_RCPP
}
// encode_native_image_rcpp_3
IntegerVector encode_native_image_rcpp_3(NumericMatrix& r_image, NumericMatrix& g_image, NumericMatrix& b_image);
RcppExport SEXP _rayimage_encode_native_image_rcpp_3(SEXP r_imageSEXP, SEXP g_imageSEXP, SEXP b_imageSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix& >::type r_image(r_imageSEXP);
    Rcpp::traits::input_parameter< NumericMatrix& >::type g_image(g_imageSEXP);
    Rcpp::traits::input_parameter< NumericMatrix& >::type b_image(b_imageSEXP);
    rcpp_result_gen = Rcpp::wrap(encode_native_image_rcpp_3(r_image, g_image, b_image));
    return rcpp_result_gen;
END_RCPP
}
// encode_native_image_rcpp_4
IntegerVector encode_native_image_rcpp_4(NumericMatrix& r_image, NumericMatrix& g_image, NumericMatrix& b_image, NumericMatrix& a_image);
RcppExport SEXP _rayimage_encode_native_image_rcpp_4(SEXP r_imageSEXP, SEXP g_imageSEXP, SEXP b_imageSEXP, SEXP a_imageSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix& >::type r_image(r_imageSEXP);
    Rcpp::traits::input_parameter< NumericMatrix& >::type g_image(g_imageSEXP);
    Rcpp::traits::input_parameter< NumericMatrix& >::type b_image(b_imageSEXP);
    Rcpp::traits::input_parameter< NumericMatrix& >::type a_image(a_imageSEXP);
    rcpp_result_gen = Rcpp::wrap(encode_native_image_rcpp_4(r_image, g_image, b_image, a_image));
    return rcpp_result_gen;
END_RCPP
}
// subsample
arma::mat subsample(arma::mat& circle, int size);
RcppExport SEXP _rayimage_subsample(SEXP circleSEXP, SEXP sizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat& >::type circle(circleSEXP);
    Rcpp::traits::input_parameter< int >::type size(sizeSEXP);
    rcpp_result_gen = Rcpp::wrap(subsample(circle, size));
    return rcpp_result_gen;
END_RCPP
}
// rayinterp2
arma::mat rayinterp2(arma::mat& image, arma::vec& XI, arma::vec& YI);
RcppExport SEXP _rayimage_rayinterp2(SEXP imageSEXP, SEXP XISEXP, SEXP YISEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat& >::type image(imageSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type XI(XISEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type YI(YISEXP);
    rcpp_result_gen = Rcpp::wrap(rayinterp2(image, XI, YI));
    return rcpp_result_gen;
END_RCPP
}
// resize_image
arma::mat resize_image(arma::mat& image, float mag);
RcppExport SEXP _rayimage_resize_image(SEXP imageSEXP, SEXP magSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat& >::type image(imageSEXP);
    Rcpp::traits::input_parameter< float >::type mag(magSEXP);
    rcpp_result_gen = Rcpp::wrap(resize_image(image, mag));
    return rcpp_result_gen;
END_RCPP
}
// resize_image_xy
arma::mat resize_image_xy(arma::mat& image, arma::vec XI, arma::vec YI);
RcppExport SEXP _rayimage_resize_image_xy(SEXP imageSEXP, SEXP XISEXP, SEXP YISEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat& >::type image(imageSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type XI(XISEXP);
    Rcpp::traits::input_parameter< arma::vec >::type YI(YISEXP);
    rcpp_result_gen = Rcpp::wrap(resize_image_xy(image, XI, YI));
    return rcpp_result_gen;
END_RCPP
}
// resize_matrix_stb
NumericMatrix resize_matrix_stb(NumericMatrix image, int width, int height, int method);
RcppExport SEXP _rayimage_resize_matrix_stb(SEXP imageSEXP, SEXP widthSEXP, SEXP heightSEXP, SEXP methodSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type image(imageSEXP);
    Rcpp::traits::input_parameter< int >::type width(widthSEXP);
    Rcpp::traits::input_parameter< int >::type height(heightSEXP);
    Rcpp::traits::input_parameter< int >::type method(methodSEXP);
    rcpp_result_gen = Rcpp::wrap(resize_matrix_stb(image, width, height, method));
    return rcpp_result_gen;
END_RCPP
}
// generate_disk
arma::mat generate_disk(float radius, int dim, bool offsetx, bool offsety);
RcppExport SEXP _rayimage_generate_disk(SEXP radiusSEXP, SEXP dimSEXP, SEXP offsetxSEXP, SEXP offsetySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< float >::type radius(radiusSEXP);
    Rcpp::traits::input_parameter< int >::type dim(dimSEXP);
    Rcpp::traits::input_parameter< bool >::type offsetx(offsetxSEXP);
    Rcpp::traits::input_parameter< bool >::type offsety(offsetySEXP);
    rcpp_result_gen = Rcpp::wrap(generate_disk(radius, dim, offsetx, offsety));
    return rcpp_result_gen;
END_RCPP
}
// gen_ellipse
arma::mat gen_ellipse(const double intensity, double width, double height);
RcppExport SEXP _rayimage_gen_ellipse(SEXP intensitySEXP, SEXP widthSEXP, SEXP heightSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const double >::type intensity(intensitySEXP);
    Rcpp::traits::input_parameter< double >::type width(widthSEXP);
    Rcpp::traits::input_parameter< double >::type height(heightSEXP);
    rcpp_result_gen = Rcpp::wrap(gen_ellipse(intensity, width, height));
    return rcpp_result_gen;
END_RCPP
}
// subsample_rect
arma::mat subsample_rect(arma::mat& rect, int binsx, int binsy);
RcppExport SEXP _rayimage_subsample_rect(SEXP rectSEXP, SEXP binsxSEXP, SEXP binsySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat& >::type rect(rectSEXP);
    Rcpp::traits::input_parameter< int >::type binsx(binsxSEXP);
    Rcpp::traits::input_parameter< int >::type binsy(binsySEXP);
    rcpp_result_gen = Rcpp::wrap(subsample_rect(rect, binsx, binsy));
    return rcpp_result_gen;
END_RCPP
}
// gen_circle_psf
arma::mat gen_circle_psf(const double radius);
RcppExport SEXP _rayimage_gen_circle_psf(SEXP radiusSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const double >::type radius(radiusSEXP);
    rcpp_result_gen = Rcpp::wrap(gen_circle_psf(radius));
    return rcpp_result_gen;
END_RCPP
}
// is_inside
bool is_inside(double sizehex, double positionx, double positiony, double sinval, double cosval);
RcppExport SEXP _rayimage_is_inside(SEXP sizehexSEXP, SEXP positionxSEXP, SEXP positionySEXP, SEXP sinvalSEXP, SEXP cosvalSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type sizehex(sizehexSEXP);
    Rcpp::traits::input_parameter< double >::type positionx(positionxSEXP);
    Rcpp::traits::input_parameter< double >::type positiony(positionySEXP);
    Rcpp::traits::input_parameter< double >::type sinval(sinvalSEXP);
    Rcpp::traits::input_parameter< double >::type cosval(cosvalSEXP);
    rcpp_result_gen = Rcpp::wrap(is_inside(sizehex, positionx, positiony, sinval, cosval));
    return rcpp_result_gen;
END_RCPP
}
// gen_hex_psf
arma::mat gen_hex_psf(const double radius, const double rotation);
RcppExport SEXP _rayimage_gen_hex_psf(SEXP radiusSEXP, SEXP rotationSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const double >::type radius(radiusSEXP);
    Rcpp::traits::input_parameter< const double >::type rotation(rotationSEXP);
    rcpp_result_gen = Rcpp::wrap(gen_hex_psf(radius, rotation));
    return rcpp_result_gen;
END_RCPP
}
// psf
arma::mat psf(const arma::mat& image, const IntegerMatrix blurmatrix, const arma::mat& depthmap, double depth, const arma::mat custombokeh, int type, double bokehintensity, double bokehlimit, double rotation, bool progbar, int channel);
RcppExport SEXP _rayimage_psf(SEXP imageSEXP, SEXP blurmatrixSEXP, SEXP depthmapSEXP, SEXP depthSEXP, SEXP custombokehSEXP, SEXP typeSEXP, SEXP bokehintensitySEXP, SEXP bokehlimitSEXP, SEXP rotationSEXP, SEXP progbarSEXP, SEXP channelSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type image(imageSEXP);
    Rcpp::traits::input_parameter< const IntegerMatrix >::type blurmatrix(blurmatrixSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type depthmap(depthmapSEXP);
    Rcpp::traits::input_parameter< double >::type depth(depthSEXP);
    Rcpp::traits::input_parameter< const arma::mat >::type custombokeh(custombokehSEXP);
    Rcpp::traits::input_parameter< int >::type type(typeSEXP);
    Rcpp::traits::input_parameter< double >::type bokehintensity(bokehintensitySEXP);
    Rcpp::traits::input_parameter< double >::type bokehlimit(bokehlimitSEXP);
    Rcpp::traits::input_parameter< double >::type rotation(rotationSEXP);
    Rcpp::traits::input_parameter< bool >::type progbar(progbarSEXP);
    Rcpp::traits::input_parameter< int >::type channel(channelSEXP);
    rcpp_result_gen = Rcpp::wrap(psf(image, blurmatrix, depthmap, depth, custombokeh, type, bokehintensity, bokehlimit, rotation, progbar, channel));
    return rcpp_result_gen;
END_RCPP
}
// convolution_cpp
arma::mat convolution_cpp(const arma::mat& image, const arma::mat kernel, bool progbar, int channel, arma::mat& bloom_matrix);
RcppExport SEXP _rayimage_convolution_cpp(SEXP imageSEXP, SEXP kernelSEXP, SEXP progbarSEXP, SEXP channelSEXP, SEXP bloom_matrixSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type image(imageSEXP);
    Rcpp::traits::input_parameter< const arma::mat >::type kernel(kernelSEXP);
    Rcpp::traits::input_parameter< bool >::type progbar(progbarSEXP);
    Rcpp::traits::input_parameter< int >::type channel(channelSEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type bloom_matrix(bloom_matrixSEXP);
    rcpp_result_gen = Rcpp::wrap(convolution_cpp(image, kernel, progbar, channel, bloom_matrix));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_rayimage_get_boolean_distance", (DL_FUNC) &_rayimage_get_boolean_distance, 1},
    {"_rayimage_encode_native_image_rcpp_1", (DL_FUNC) &_rayimage_encode_native_image_rcpp_1, 1},
    {"_rayimage_encode_native_image_rcpp_3", (DL_FUNC) &_rayimage_encode_native_image_rcpp_3, 3},
    {"_rayimage_encode_native_image_rcpp_4", (DL_FUNC) &_rayimage_encode_native_image_rcpp_4, 4},
    {"_rayimage_subsample", (DL_FUNC) &_rayimage_subsample, 2},
    {"_rayimage_rayinterp2", (DL_FUNC) &_rayimage_rayinterp2, 3},
    {"_rayimage_resize_image", (DL_FUNC) &_rayimage_resize_image, 2},
    {"_rayimage_resize_image_xy", (DL_FUNC) &_rayimage_resize_image_xy, 3},
    {"_rayimage_resize_matrix_stb", (DL_FUNC) &_rayimage_resize_matrix_stb, 4},
    {"_rayimage_generate_disk", (DL_FUNC) &_rayimage_generate_disk, 4},
    {"_rayimage_gen_ellipse", (DL_FUNC) &_rayimage_gen_ellipse, 3},
    {"_rayimage_subsample_rect", (DL_FUNC) &_rayimage_subsample_rect, 3},
    {"_rayimage_gen_circle_psf", (DL_FUNC) &_rayimage_gen_circle_psf, 1},
    {"_rayimage_is_inside", (DL_FUNC) &_rayimage_is_inside, 5},
    {"_rayimage_gen_hex_psf", (DL_FUNC) &_rayimage_gen_hex_psf, 2},
    {"_rayimage_psf", (DL_FUNC) &_rayimage_psf, 11},
    {"_rayimage_convolution_cpp", (DL_FUNC) &_rayimage_convolution_cpp, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_rayimage(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
