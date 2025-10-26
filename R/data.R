#' Dragon Image
#'
#' @format An RGB 3-layer HDR rayimg array with 200 rows and 200 columns,
#' generated using the rayrender package, with gamma correction.
"dragon"

#' Dragon Depthmap
#'
#' @format An matrix with 200 rows and 200 columns, representing
#' the depth into the `dragon` image scene. Generated using the rayrender package.
#' Distances range from 847 to 1411.
"dragondepth"

#' Sunset Image
#'
#' @format An RGBA 4-layer LDR rayimg array from a JPEG image
#' with 400 rows and 400 columns.
#' @author Tyler Morgan-Wall
"sunset_image"

#' ACEScg conversion matrices
#'
#' @format Length-2 list of 3x3 matrices
"acescg_conversion_matrices"

#' sRGB conversion matrices
#'
#' @format Length-2 list of 3x3 matrices
"srgb_conversion_matrices"
