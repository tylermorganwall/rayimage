
# rayimage

<img src="man/figures/githubdemo.gif" ></img>

## Overview

**rayimage** is an open source R package for image manipulation and
simulated camera effects. **rayfocus** uses convolution-based techniques
to generate simulated camera bokeh, depth of field, and other camera
effects, using an image and an optional depth map. It includes functions
to perform 2D convolutions, add image overlays, generate camera vignette
effects, and add titles to images.

## Installation

``` r
# To install the latest version from Github:
# install.packages("remotes")
remotes::install_github("tylermorganwall/rayimage")
```

## Functions

  - `render_bokeh()` takes two images as an input: an in-focus image,
    and a depth map for that image. These can either be in-memory
    representations of the image, or file paths to the images. The user
    can also specify various camera settings, including: focal length,
    f-stop, aperture shape (including custom aperture shapes passed by
    the user), aperture rotation, bokeh intensity, bokeh intensity
    limit. The output is either plotted to the current device, or save
    to a file (if a filename is passed to the function).

  - `render_convolution()` performs a discrete convolution with a
    user-supplied kernel (either custom, or using one of the built-in
    functions to generate a kernel: `generate_2d_gaussian()`,
    `generate_2d_exponential()`, and `generate_2d_disk()`). This
    function can be applied to either images represented by RGB
    arrays/filenames, or 2D matrices.

  - `render_resized()` resizes an image or a matrix using a
    magnification factor or the dimensions of the desired object. This
    function interpolates between points using bilinear interpolation.

  - `render_reorient()` reorients an image, either flipping
    horizontally, vertically, or with a transpose operation.

  - `add_title()` adds titles to images, with an optional title bar.
    User can specify opacity, color, and font properties.

  - `add_vignette()` adds a camera vignette effect, which is a slight
    darkening by the edges of an image.

  - `add_image_overlay()` adds overlays to images.

  - `plot_image()` plots an RGB array to the current device.

## Usage

The package comes with a sample image and depth map derived from
Stanford “Chinese Dragon” 3D model. The image is included in rayimage as
`dragon` and the depthmap is `dragondepth`.

``` r
library(rayimage)

par(mfrow = c(1,2))

plot_image(dragon)
image(dragondepth, col = hcl.colors(256, "YlOrRd", rev = TRUE), 
      axes=FALSE, asp=1)
```

![](man/figures/unnamed-chunk-1-1.png)<!-- -->

Preview the focal plane.

``` r
par(mfrow = c(1,2))
render_bokeh(dragon,dragondepth,focus=930,preview_focus = TRUE)
```

    ## [1] "Focal range: 847.644-1410.17"

``` r
render_bokeh(dragon,dragondepth,focus=930,focallength=250)
```

![](man/figures/unnamed-chunk-2-1.png)<!-- -->

``` r
render_bokeh(dragon,dragondepth,focus=1300,preview_focus = TRUE)
```

    ## [1] "Focal range: 847.644-1410.17"

``` r
render_bokeh(dragon,dragondepth,focus=1300,focallength=250)
```

![](man/figures/unnamed-chunk-2-2.png)<!-- -->

We can also adjust the focal point, shape of the aperture, f-stop, focal
length, as well as the bokeh intensity.

``` r
par(mfrow = c(1,2))
render_bokeh(dragon,dragondepth,focus=1100,focallength = 400,
             bokehshape = "hex", bokehintensity = 1)
render_bokeh(dragon,dragondepth,focus=900,focallength = 400,
             bokehshape = "hex", bokehintensity = 1)
```

![](man/figures/unnamed-chunk-3-1.png)<!-- -->

``` r
render_bokeh(dragon,dragondepth,focus=900,focallength = 400,
             fstop = 16, bokehshape = "hex")
render_bokeh(dragon,dragondepth,focus=900,focallength = 300,
             bokehshape = "hex", bokehintensity = 5)
```

![](man/figures/unnamed-chunk-3-2.png)<!-- -->

We can add a camera vignette effect, titles, and add overlays (not shown
here):

``` r
par(mfrow = c(1,2))

dragon |>
  add_title("Dragon", title_size = 20, title_bar_color = "red", 
            title_bar_alpha=0.8, title_color="white", title_offset = c(12,12)) |>
  plot_image()

dragon |>
  add_vignette(vignette=0.8) |>
  plot_image()
```

![](man/figures/unnamed-chunk-4-1.png)<!-- -->

We can also resize images and matrices with `render_resize()`. We can do
this before adding text to make the resulting text smoother.

``` r
par(mfrow = c(1,2))

#Double the input size to make the resulting text smoother.
dragon |>
  render_resized(mag = 2) |>
  add_title("Dragon", title_size = 20, title_bar_color = "red", 
            title_bar_alpha=0.8, title_color="white", title_offset = c(12,12)) |>
  plot_image()

#Specify resulting dimensions directly.
dragon |>
  render_resized(dim = c(600,300)) |>
  add_title("Dragon", title_size = 20, title_bar_color = "red", 
            title_bar_alpha=0.8, title_color="white", title_offset = c(12,12)) |>
  plot_image()
```

![](man/figures/unnamed-chunk-5-1.png)<!-- -->

Reorienting images and matrices can be done with `render_reorient()`:

``` r
par(mfrow = c(2,2))
render_reorient(dragon, preview = TRUE)
render_reorient(dragon, flipx = TRUE, preview = TRUE)
render_reorient(dragon, flipy = TRUE, preview = TRUE)
render_reorient(dragon, transpose = TRUE, preview = TRUE)
```

![](man/figures/unnamed-chunk-6-1.png)<!-- -->

We can also use the `render_convolution()` directly to perform a
convolution with a user-defined (or built-in) kernel.

``` r
par(mfrow = c(1,2))
#Default gaussian kernel
render_convolution(dragon, kernel = "gaussian", preview = TRUE)
image(generate_2d_gaussian(1,1,11,3), asp=1, axes=FALSE)
```

![](man/figures/unnamed-chunk-7-1.png)<!-- -->

``` r
#Custom gaussian kernel
render_convolution(dragon, kernel = generate_2d_gaussian(10,1,31,21), preview = TRUE) 
image(generate_2d_gaussian(10,1,31,21), asp=1, axes=FALSE)
```

![](man/figures/unnamed-chunk-7-2.png)<!-- -->

``` r
#Custom exponential kernel
render_convolution(dragon, kernel = generate_2d_exponential(3,31,21), preview = TRUE)
image(generate_2d_exponential(3,31,21), asp=1, axes=FALSE)
```

![](man/figures/unnamed-chunk-7-3.png)<!-- -->

``` r
#Custom disk kernel
render_convolution(dragon, kernel = generate_2d_disk(31), preview = TRUE)
image(generate_2d_disk(31), asp=1, axes=FALSE)
```

![](man/figures/unnamed-chunk-7-4.png)<!-- -->

We can also use this to perform generate discrete 2D convolutions with
matrices:

``` r
par(mfrow = c(1,2))

volcano |> image()
volcano |> 
  render_convolution(kernel=generate_2d_gaussian(sd=1,dim=31)) |> 
  image()
```

![](man/figures/unnamed-chunk-8-1.png)<!-- -->

And here we use a user-defined kernel, in the shape of a cross.

``` r
par(mfrow = c(2,2))
custom1 = matrix(0, nrow=11,ncol=11)
custom1[6,] = 1
custom1[,6] = 1
custom2 = diag(10) + (diag(10)[,10:1])
plot_image(custom1)
plot_image(custom2)

#Horizontal cross
render_convolution(dragon, kernel = custom1, preview = TRUE) 
#Diagonal cross
render_convolution(dragon, kernel = custom2, preview = TRUE)
```

![](man/figures/unnamed-chunk-9-1.png)<!-- -->
