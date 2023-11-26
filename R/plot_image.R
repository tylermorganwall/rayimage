#'@title Plot Image
#'
#'@description Displays the image in the current device.
#'
#'@param input Image or filename of an image to be plotted.
#'@param rotate Default 0. Rotates the output. Possible values: 0, 90, 180, 270.
#'@param draw_grid Default `FALSE`. If `TRUE`, this will draw a grid in the background to help
#'disambiguate the actual image from the device (helpful if the image background is the same as the
#'device's background).
#'@param asp Default `1`. Aspect ratio of the pixels in the plot. For example, an aspect ratio of `4/3` will
#'slightly widen the image.
#'@param new_page  Default `TRUE`. Whether to call `grid::grid.newpage()` before plotting the image.
#'@param return_grob Default `FALSE`. Whether to return the grob object.
#'@export
#'@examples
#'#if(interactive()){
#'#Plot the dragon array
#'plot_image(dragon)
#'#Make pixels twice as wide as tall
#'plot_image(dragon, asp = 2)
#'#Plot non-square images
#'plot_image(dragon[1:100,,])
#'#Make pixels twice as tall as wide
#'plot_image(dragon[1:100,,], asp = 1/2)
#'#end}
plot_image = function(input, rotate=0, draw_grid = FALSE,
                      asp = 1, new_page = TRUE, return_grob = FALSE) {
  imagetype = get_file_type(input)
  if(imagetype == "jpg") {
    input = suppressWarnings(jpeg::readJPEG(input))
  } else if (imagetype == "png"){
    input = suppressWarnings(png::readPNG(input))
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

    if(new_page) {
      grid::grid.newpage()
    }

    # Draw a grid to differentiate image from background
    if(draw_grid) {
      draw_grid_fxn = function() {
        grid::pushViewport(
          grid::viewport(layout = grid::grid.layout(1, 1,
                                                    widths = grid::unit(1, "npc"),
                                                    heights = grid::unit(1, "npc")))
        )
        # Define grid density and angle
        grid_density = 0.01 # Adjust this value for tighter or looser grid
        for (i in seq(-2, 2, by = grid_density)) {
          grid::grid.lines(x = c(0, 1), y = c(i, i + 1), default.units = "npc", gp = grid::gpar(col = "grey"))
          grid::grid.lines(x = c(0, 1), y = c(i, i - 1), default.units = "npc", gp = grid::gpar(col = "grey"))
        }
        grid::popViewport()
      }
      draw_grid_fxn()
    }
    return(plot_asp_native_raster(nr, asp = asp, return_grob = return_grob))
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
    array_from_mat = array(input,dim=c(nrow(input),ncol(input),3))
    nr = convert_to_native_raster(array_from_mat)

    if(new_page) {
      grid::grid.newpage()
    }
    return(plot_asp_native_raster(nr, asp = asp, return_grob = return_grob))
  } else {
    stop("`input` is neither array nor matrix--convert to either to plot.")
  }
}
