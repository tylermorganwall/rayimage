% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_image_grid.R
\name{plot_image_grid}
\alias{plot_image_grid}
\title{Plot Image Grid}
\usage{
plot_image_grid(input_list, dim = c(1, 1), asp = 1, draw_grid = FALSE)
}
\arguments{
\item{input_list}{List of array (or matrix) image inputs.}

\item{dim}{Default \code{c(1,1)}. Width by height of output grid.}

\item{asp}{Default \code{1}. Aspect ratio of the pixels(s). For example, an aspect ratio of \code{4/3} will
slightly widen the image. This can also be a vector the same length of
\code{input_list} to specify an aspect ratio for each image in the grid.}

\item{draw_grid}{Default \code{FALSE}. If \code{TRUE}, this will draw a grid in the background to help
disambiguate the actual image from the device (helpful if the image background is the same as the
device's background).}
}
\description{
Displays the image in the current device.
}
\examples{
if(run_documentation()){
#Plot the dragon array
plot_image_grid(list(dragon, 1-dragon), dim = c(1,2))
}
if(run_documentation()){
plot_image_grid(list(dragon, 1-dragon), dim = c(2,1))
}
if(run_documentation()){
plot_image_grid(list(dragon, NULL, 1-dragon), dim = c(2,2), asp = c(2,1,1/2))
}
if(run_documentation()){
plot_image_grid(list(dragon, NULL, NULL, dragon), dim = c(2,2), asp = c(2,1,1,1/2))
}
if(run_documentation()){
#Plot alongside the depth matrix
dragon_depth_reoriented = render_reorient(dragondepth,
                                         transpose = TRUE,
                                         flipx = TRUE)/2000
plot_image_grid(list(dragondepth/2000, dragon, dragon, dragondepth/2000),
               dim = c(2,2))
}
}
