#'@title Plot Image
#'
#'@description Displays the image in the current device.
#'
#'@param input Image or filename of an image to be plotted.
#'@param rotate Default 0. Rotates the output. Possible values: 0, 90, 180, 270.
#'@param keep_user_par Default `TRUE`. Whether to keep the user's `par()` settings. Set to `FALSE` if you
#'want to set up a multi-pane plot (e.g. set `par(mfrow)`).
#'@param ... Additional arguments to pass to the `raster::plotRGB` function that displays the map.
#'@export
#'@examples
#'#Plot the dragon array
#'plot_image(dragon)
plot_image = function(input, rotate=0, keep_user_par = FALSE, ...) {
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
    suppressWarnings(raster::plotRGB(raster::brick(input, xmn = 0.5, xmx = dim(input)[2]+ 0.5,ymn = 0.5, ymx = dim(input)[1] + 0.5, ...),scale=1,  asp=1, maxpixels=3*nrow(input)*ncol(input),...))
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
    array_from_mat = array(flipud(t(input)),dim=c(ncol(input),nrow(input),3))
    suppressWarnings(raster::plotRGB(raster::brick(array_from_mat, xmn = 0.5, xmx = dim(array_from_mat)[2] + 0.5,ymn =  0.5, ymx = dim(array_from_mat)[1] +  0.5, ...),scale=1,asp=1, maxpixels=3*nrow(input)*ncol(input), ...))
  } else {
    stop("`input` is neither array nor matrix--convert to either to plot.")
  }
}
