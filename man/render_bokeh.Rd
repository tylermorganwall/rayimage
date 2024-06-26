% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/render_bokeh.R
\name{render_bokeh}
\alias{render_bokeh}
\title{Render Bokeh}
\usage{
render_bokeh(
  image,
  depthmap,
  focus = 0.5,
  focallength = 100,
  fstop = 4,
  filename = NULL,
  preview = TRUE,
  preview_focus = FALSE,
  bokehshape = "circle",
  bokehintensity = 1,
  bokehlimit = 0.8,
  rotation = 0,
  aberration = 0,
  gamma_correction = TRUE,
  progress = interactive(),
  ...
)
}
\arguments{
\item{image}{Image filename or 3-layer RGB array.}

\item{depthmap}{Depth map filename or 1d array.}

\item{focus}{Defaults \code{0.5}. Depth in which to blur.}

\item{focallength}{Default \code{100}. Focal length of the virtual camera.}

\item{fstop}{Default \code{4}. F-stop of the virtual camera.}

\item{filename}{Default \code{NULL}. The filename of the image to be saved. If this is not given, the image will be plotted instead.}

\item{preview}{Default \code{TRUE}. If \code{FALSE}, it will not display the image and just return the RGB array.}

\item{preview_focus}{Default \code{FALSE}. If \code{TRUE}, a red line will be drawn across the image
showing where the camera will be focused.}

\item{bokehshape}{Default \code{circle}. Also built-in: \code{hex}. The shape of the bokeh. If the user
passes in a 2D matrix, that matrix will control the shape of the bokeh.}

\item{bokehintensity}{Default \code{1}. Intensity of the bokeh when the pixel intensity is greater than \code{bokehlimit}.}

\item{bokehlimit}{Default \code{0.8}. Limit after which the bokeh intensity is increased by \code{bokehintensity}.}

\item{rotation}{Default \code{0}. Number of degrees to rotate the hexagonal bokeh shape.}

\item{aberration}{Default \code{0}. Adds chromatic aberration to the image. Maximum of \code{1}.}

\item{gamma_correction}{Default \code{TRUE}. Controls gamma correction when adding colors. Default exponent of 2.2.}

\item{progress}{Default \code{TRUE}. Whether to display a progress bar.}

\item{...}{Additional arguments to pass to \code{plot_image()} if \code{preview = TRUE}.}
}
\value{
3-layer RGB array of the processed image.
}
\description{
Takes an image and a depth map to render the image with depth of field
(i.e. similar to "Portrait Mode" in an iPhone). User can specify a custom bokeh shape,
or use one of the built-in bokeh types.
}
\examples{
if(run_documentation()){
#Plot the dragon
plot_image(dragon)
}
if(run_documentation()){
#Plot the depth map
plot_image(dragondepth/1500)
}
if(run_documentation()){
#Preview the focal plane:
render_bokeh(dragon, dragondepth, focus=950, preview_focus = TRUE)
}
if(run_documentation()){
#Change the focal length:
render_bokeh(dragon, dragondepth, focus=950, focallength=300)
}
if(run_documentation()){
#Add chromatic aberration:
render_bokeh(dragon, dragondepth, focus=950, focallength=300, aberration = 0.5)
}
if(run_documentation()){
#Change the focal distance:
render_bokeh(dragon, dragondepth, focus=600, focallength=300)
render_bokeh(dragon, dragondepth, focus=1300, focallength=300)
}
if(run_documentation()){
#Change the bokeh shape to a hexagon:
render_bokeh(dragon, dragondepth, bokehshape = "hex",
            focallength=300, focus=600)
}
if(run_documentation()){
#Change the bokeh intensity:
render_bokeh(dragon, dragondepth,
            focallength=400, focus=900, bokehintensity = 1)
render_bokeh(dragon, dragondepth,
            focallength=400, focus=900, bokehintensity = 3)
}
if(run_documentation()){
#Rotate the hexagonal shape:
render_bokeh(dragon, dragondepth, bokehshape = "hex", rotation=15,
            focallength=300, focus=600)
}
}
