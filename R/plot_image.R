#'@title Plot Image
#'
#'@description Displays the image in the current device.
#'
#'@param input Image or filename of an image to be plotted.
#'@param rotate Default 0. Rotates the output. Possible values: 0, 90, 180, 270.
#'@param keep_user_par Default `TRUE`. Whether to keep the user's `par()` settings. Set to `FALSE` if you
#'want to set up a multi-pane plot (e.g. set `par(mfrow)`).
#'@param asp Default `1`. Aspect ratio of the pixels in the plot. For example, an aspect ratio of `4/3` will
#'slightly widen the image.
#'@param new_page  Default `TRUE`. Whether to call `grid::grid.newpage()` before plotting the image.
#'@param ... Additional arguments to pass to the `raster::plotRGB` function that displays the map.
#'@export
#'@examples
#'#if(interactive()){
#'#Plot the dragon array
#'plot_image(dragon)
#'#end}
plot_image = function(input, rotate=0, keep_user_par = FALSE,
                      asp = 1, new_page = TRUE, ...) {
  imagetype = get_file_type(input)
  if(imagetype == "jpg") {
    input = suppressWarnings(jpeg::readJPEG(input))
  } else if (imagetype == "png"){
    input = suppressWarnings(png::readPNG(input))
  }
  if(keep_user_par) {
    old.par = graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old.par))
  }
  rotatef = function(x) t(apply(x, 2, rev))
  if(!(rotate %in% c(0,90,180,270))) {
    if(length(rotate) == 1) {
      warning(paste0("Rotation value ",rotate," not in c(0,90,180,270). Ignoring"))
    } else {
      warning(paste0("Rotation argument `rotate` not in c(0,90,180,270). Ignoring"))
    }
    number_of_rots = 0
  } else {
    number_of_rots = rotate/90
  }
  if(length(dim(input)) == 3) {
    if(dim(input)[3] == 2) {
      temparray = array(1,dim = c(dim(input)[1:2],3))
      temparray[,,1:3] = input[,,1]
      input = temparray
    }
    if(number_of_rots != 0) {
      newarray = input
      newarrayt = array(0,dim=c(ncol(input),nrow(input),3))
      for(i in 1:number_of_rots) {
        for(j in 1:3) {
          if(i == 2) {
            newarray[,,j] = rotatef(newarrayt[,,j])
          } else {
            newarrayt[,,j] = rotatef(newarray[,,j])
          }
        }
      }
      if(number_of_rots == 2) {
        input = newarray
      } else {
        input = newarrayt
      }
    }
    if(any(input > 1 | input < 0,na.rm = TRUE)) {
      input[input > 1] = 1
      input[input < 0] = 0
    }
    nr = convert_to_native_raster(input)
    xlim = dim(input)[2]
    ylim = dim(input)[1]

    if(new_page) {
      grid::grid.newpage()
    }
    flip = FALSE
    if(min(1,xlim/ylim)*asp > 1) {
      flip = TRUE
    }
    if(flip) {
      grid::grid.raster(nr, interpolate = FALSE,
                        x=0.5, y=0.5,
                        width=grid::unit(min(1,ylim/xlim), "snpc"),
                        height=grid::unit(min(1,ylim/xlim)/asp, "snpc"))
    } else {
      grid::grid.raster(nr, interpolate = FALSE,
                        width=grid::unit(min(1,xlim/ylim)*asp, "snpc"),
                        height=grid::unit(min(1,ylim/xlim), "snpc"))
    }
  } else if(length(dim(input)) == 2) {
    if(number_of_rots != 0) {
      for(j in 1:number_of_rots) {
        input = rotatef(input)
      }
    }
    if(any(input > 1 | input < 0,na.rm = TRUE)) {
      input[input > 1] = 1
      input[input < 0] = 0
    }
    xlim = dim(input)[2]
    ylim = dim(input)[1]
    array_from_mat = array(input,dim=c(nrow(input),ncol(input),3))
    nr = convert_to_native_raster(array_from_mat)
    if(new_page) {
      grid::grid.newpage()
    }
    flip = FALSE
    if(min(1,xlim/ylim)*asp > 1) {
      flip = TRUE
    }
    if(flip) {
      grid::grid.raster(nr, interpolate = FALSE,
                        x=0.5, y=0.5,
                        width=grid::unit(min(1,ylim/xlim), "snpc"),
                        height=grid::unit(min(1,ylim/xlim)/asp, "snpc"))
    } else {
      grid::grid.raster(nr, interpolate = FALSE,
                        width=grid::unit(min(1,xlim/ylim)*asp, "snpc"),
                        height=grid::unit(min(1,ylim/xlim), "snpc"))
    }
  } else {
    stop("`input` is neither array nor matrix--convert to either to plot.")
  }
}
