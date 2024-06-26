% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/render_clamp.R
\name{render_clamp}
\alias{render_clamp}
\title{Clamp Image}
\usage{
render_clamp(image, min_value = 0, max_value = 1, preview = FALSE, ...)
}
\arguments{
\item{image}{Image filename or 3-layer RGB array.}

\item{min_value}{Default \code{0}. Minimum value to clamp the image to.}

\item{max_value}{Default \code{1}. Maximum value to clamp the image to.}

\item{preview}{Default \code{FALSE}. If \code{TRUE}, it will display the image in addition
to returning it.}

\item{...}{Arguments to pass to either \code{jpeg::readJPEG}, \code{png::readPNG}, or \code{tiff::readTIFF}.}
}
\value{
3-layer RGB array of the processed image.
}
\description{
Clamps an image to a user-specified range
}
\examples{
if(run_documentation()){
#The rnage of the unchanged image
range(dragon)
}
if(run_documentation()){
#Clamp the maximum and minimum values to one and zero
render_clamp(dragon) |>
 range()
}
}
